#' Get fisheries catch data
#'
#' Get fisheries catch data from DNR lake finder website using a set of DOW identifiers
#'
#' @param dows numeric vector of lake DOWs
#' @param trace logical if progress is printed in console
#' @param rich.out logical if species richness is also returned
#'
#' @export
#'
#' @import XML
#'
#' @return a list or something, I don't know
#'
#' @examples
#'
#' dows <- c('27013700', '47004900')
#'
#' catch_data(dows)
#'
catch_data <- function(dows,trace=TRUE,rich.out=FALSE){

  strt<-Sys.time()

  # add leading zero if nchar 7
  dows[nchar(dows) == 7] <- paste0('0', dows[nchar(dows) == 7])

  #create object for output
  fish.out<-vector('list',length(dows))
  names(fish.out)<-dows

  for(dow in dows){

    if(trace){
      cat(paste(dow,'; ',which(dow==dows),' of ',length(dows),sep=''),'\n')
      flush.console()
    }

    #get raw html
    html<-htmlTreeParse(
      paste(
        'http://www.dnr.state.mn.us/lakefind/showreport.html?downum=',
        dow,
        sep=''
      ),
      useInternalNodes=TRUE
    )

    #no page for dow
    check.site<-unlist(xpathApply(html, "//h3", xmlValue))
    if(length(grep('Fish Sampled*',check.site))==0){
      fish.out[dow]<-'No survey'
      next
    }

    #page exists, find survey data if available
    fish.tab<-unlist(xpathApply(html, "//table", xmlValue))
    abun.ind<-grep('Species\nGear*',fish.tab)
    leng.ind<-grep('Species\nNumber*',fish.tab)

    #no survey data
    if(grepl('No fish were collected',fish.tab[abun.ind])){
      fish.out[dow]<-'No survey'
      next
    }

    #no survey data, different table format
    if(length(strsplit(fish.tab[abun.ind],'\n')[[1]])==7){
      fish.out[dow]<-'No survey'
      next
    }

    if(class(try({

      smp.date<-{
        tmp<-check.site[grep('Fish Sampled*',check.site)]
        strs<-gregexpr("[0-9]",tmp)[[1]]
        substr(tmp,strs[1],strs[length(strs)])
      }

      abun.tab<-{

        tmp<-fish.tab[abun.ind]
        tmp.prs<-strsplit(tmp,'\n')[[1]]

        tmp.dat<-matrix(tmp.prs[8:length(tmp.prs)],ncol=6,byrow=TRUE)[,c(1:3,5),drop=FALSE]
        blank.spp<-nchar(as.character(tmp.dat[,1]))<3
        tmp.dat[blank.spp,1]<-tmp.dat[which(blank.spp)-1,1]
        tmp.dat<-data.frame(tmp.dat,stringsAsFactors=FALSE)
        names(tmp.dat)<-c('Species','Net','Caught','Ave_wt')
        tmp.dat

      }

      #length data absent
      if(length(leng.ind)==0) leng.tab<-'No length data'

      else{
        leng.tab<-{

          tmp<-fish.tab[leng.ind]
          tmp.prs<-strsplit(tmp,'\n')[[1]]
          tmp.dat<-matrix(tmp.prs[12:length(tmp.prs)],ncol=10,byrow=TRUE)[,,drop=FALSE]
          tmp.dat<-data.frame(tmp.dat,stringsAsFactors=FALSE)
          names(tmp.dat)<-c('Species',tmp.prs[3:11])
          tmp.dat

        }
      }

    }))=='try-error'){

      fish.out[[dow]]<-'massive failure'
      next

    }

    fish.out[[dow]]<-list(date=smp.date,abundance=abun.tab,length=leng.tab)

  }

  print(Sys.time()-strt)

  #format output as species only
  if(rich.out)
    return(lapply(
      fish.out,
      function(x){
        if(length(x)==1) x
        else list(date=x[[1]],species=sort(unique(x[[2]][,'Species'])))
      }
    ))

  fish.out

}
