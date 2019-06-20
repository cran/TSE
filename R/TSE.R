#' @title Calculate average mean absolute error (aMAE)
#' @description Calculates average mean absolute error (aMAE) for one
#' or more surveys
#' @param Actuals1 = data from a "gold standard" survey; objects are variable columns
#' from "gold standard" survey that corruspond to variable columns Observed1
#' @param Observed1 = data from survey 1; objects are variable columns from
#' survey 1 that corruspond to variable columns from Actuals1
#' @param ... = "gold standard" data/survey # data for additional surveys
#' @return Average mean absolute error (aMAE)
#' @details aMAE for survey # => mean value of the MAEs for specified variables in
#' survey # => mean value of MAEs for objects in Observed#=data.frame()
#' @examples AVEMAE(Actuals1=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed1=data.frame(TESTNUMB$O1Q1, TESTNUMB$O1Q2),
#' Actuals2=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed2=data.frame(TESTNUMB$O2Q1, TESTNUMB$O2Q2),
#' Actuals3=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed3=data.frame(TESTNUMB$O3Q1, TESTNUMB$O3Q2))
#' @note Make sure to properly order inputs, per the example: Actuals1=data.frame()
#' objects and corrusponding Observed1=data.frame() objects must be given in the
#' same order as each other; and ... must be given in numbered pairs of Actuals#,
#' Observed#, and those pairs given in sequence of their #s.
#' @export AVEMAE
AVEMAE=function(Actuals1=data.frame(), Observed1=data.frame(), ...){
  #create lists from ...
    other=list(...)
    otherActuals=cbind(other[grepl("Actuals",names(other))])
    otherObserved=cbind(other[grepl("Observed",names(other))])
  #create storage vector
    avemaevec=vector(length=(length(otherObserved)+1))
  #calculate and store data set 1 values
    avemae=mean(apply(abs(Actuals1-Observed1), 2, mean))
    avemaevec[1]=avemae
  #calculate and store ... values
    for(i in 1:length(otherObserved)){
      avemaemore=mean(apply(abs(otherActuals[[i]]-
        otherObserved[[i]]), 2, mean))
      avemaevec[i+1]=avemaemore
    }
  #convert to matrix and label
    avemaematrix=noquote(cbind(matrix(format(avemaevec, signif=7))))
    rownames(avemaematrix)=paste("   survey",
      seq(along=avemaematrix), " => ")
    colnames(avemaematrix)=c("aMAE")
  #return results
    avemaematrix
}

#' @title Calculate average mean squared error (aMSE) with bias-variance decomposition
#' @description Calculates average mean squared error (aMSE) with bias-variance decomposition
#' for one or more surveys
#' @param Actuals1 = data from a "gold standard" survey; objects are variable columns
#' from "gold standard" survey that corruspond to variable columns Observed1
#' @param Observed1 = data from survey 1; objects are variable columns from
#' survey 1 that corruspond to variable columns from Actuals1
#' @param ... = "gold standard" data/survey # data for additional surveys
#' @return Average mean squared error (aMSE) with bias-variance decomposition
#' @details aMSE for survey # => mean value of the MSEs for specified variables in
#' survey # => mean value of MSEs for objects in Observed#=data.frame()
#' @examples AVEMSE(Actuals1=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed1=data.frame(TESTNUMB$O1Q1, TESTNUMB$O1Q2),
#' Actuals2=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed2=data.frame(TESTNUMB$O2Q1, TESTNUMB$O2Q2),
#' Actuals3=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed3=data.frame(TESTNUMB$O3Q1, TESTNUMB$O3Q2))
#' @note Make sure to properly order inputs, per the example: Actuals1=data.frame()
#' objects and corrusponding Observed1=data.frame() objects must be given in the
#' same order as each other; and ... must be given in numbered pairs of Actuals#,
#' Observed#, and those pairs given in sequence of their #s.
#' @export AVEMSE
AVEMSE=function(Actuals1=data.frame(), Observed1=data.frame(), ...){
  #create lists from ...
    other=list(...)
    otherActuals=cbind(other[grepl("Actuals",names(other))])
    otherObserved=cbind(other[grepl("Observed",names(other))])
  #create storage vector
    avemsevec=vector(length=(length(otherObserved)+1))
    avebias2vec=vector(length=(length(otherObserved)+1))
    avevarvec=vector(length=(length(otherObserved)+1))
    addvec=vector(length=(length(otherObserved)+1))
    addvec[1:(length(otherObserved)+1)]=" + "
    eqvec=vector(length=(length(otherObserved)+1))
    eqvec[1:(length(otherObserved)+1)]=" => "
  #calculate and store data set 1 values
    avemse=mean(apply(((Actuals1-Observed1)^2), 2, mean))
    avemsevec[1]=avemse
    avebias2=mean((apply((Actuals1-Observed1), 2, mean))^2)
    avebias2vec[1]=avebias2
    avevar=mean(apply(((mapply('-', (Actuals1-Observed1),
      (apply((Actuals1-Observed1), 2, mean))))^2), 2, mean))
    avevarvec[1]=avevar
  #calculate and store ... values
    for(i in 1:length(otherObserved)){
      avemsemore=mean(apply(((otherActuals[[i]]-
        otherObserved[[i]])^2), 2, mean))
      avemsevec[i+1]=avemsemore
      avebias2more=mean((apply((otherActuals[[i]]-
        otherObserved[[i]]), 2, mean))^2)
      avebias2vec[i+1]=avebias2more
      avevarmore=mean(apply(((mapply('-', (otherActuals[[i]]-
        otherObserved[[i]]), (apply((otherActuals[[i]]-
        otherObserved[[i]]), 2, mean))))^2), 2, mean))
      avevarvec[i+1]=avevarmore
    }
  #convert to matrix and label
    avemsematrix=noquote(matrix(cbind(format(avemsevec, signif=7), eqvec,
      format(avebias2vec, signif=7), addvec, format(avevarvec,
      signif=7)), ncol=5))
    rownames(avemsematrix)=paste("   survey",
      seq(along=avemsematrix[, 1]), " => ")
    colnames(avemsematrix)=c("aMSE", " ", "aBias^2", " ", "aVar")
  #return results
    avemsematrix
}

#' @title Calculate average root mean squared error (aRMSE)
#' @description Calculates average root mean squared error (aRMSE) for one
#' or more surveys
#' @param Actuals1 = data from a "gold standard" survey; objects are variable columns
#' from "gold standard" survey that corruspond to variable columns Observed1
#' @param Observed1 = data from survey 1; objects are variable columns from
#' survey 1 that corruspond to variable columns from Actuals1
#' @param ... = "gold standard" data/survey # data for additional surveys
#' @return Average root mean squared error (aRMSE)
#' @details aRMSE for survey # => mean value of the RMSEs for specified variables in
#' survey # => mean value of RMSEs for objects in Observed#=data.frame()
#' @examples AVERMSE(Actuals1=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed1=data.frame(TESTNUMB$O1Q1, TESTNUMB$O1Q2),
#' Actuals2=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed2=data.frame(TESTNUMB$O2Q1, TESTNUMB$O2Q2),
#' Actuals3=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed3=data.frame(TESTNUMB$O3Q1, TESTNUMB$O3Q2))
#' @note Make sure to properly order inputs, per the example: Actuals1=data.frame()
#' objects and corrusponding Observed1=data.frame() objects must be given in the
#' same order as each other; and ... must be given in numbered pairs of Actuals#,
#' Observed#, and those pairs given in sequence of their #s.
#' @export AVERMSE
AVERMSE=function(Actuals1=data.frame(), Observed1=data.frame(), ...){
  #create lists from ...
    other=list(...)
    otherActuals=cbind(other[grepl("Actuals",names(other))])
    otherObserved=cbind(other[grepl("Observed",names(other))])
  #create storage vector
    avermsevec=vector(length=(length(otherObserved)+1))
  #calculate and store data set 1 values
    avermse=mean(sqrt(apply(((Actuals1-Observed1)^2), 2, mean)))
    avermsevec[1]=avermse
  #calculate and store ... values
    for(i in 1:length(otherObserved)){
      avermsemore=mean(sqrt(apply(((otherActuals[[i]]-
        otherObserved[[i]])^2), 2, mean)))
      avermsevec[i+1]=avermsemore
    }
  #convert to matrix and label
    avermsematrix=noquote(matrix(cbind(format(avermsevec, signif=7))))
    rownames(avermsematrix)=paste("   survey",
      seq(along=avermsematrix), " => ")
    colnames(avermsematrix)=c("aRMSE")
  #return results
    avermsematrix
}

#' @title Calculate average mean squared logarithmic error (aMSLE)
#' @description Calculates average mean squared logarithmic error (aMSLE) for one
#' or more surveys
#' @param Actuals1 = data from a "gold standard" survey; objects are variable columns
#' from "gold standard" survey that corruspond to variable columns Observed1
#' @param Observed1 = data from survey 1; objects are variable columns from
#' survey 1 that corruspond to variable columns from Actuals1
#' @param ... = "gold standard" data/survey # data for additional surveys
#' @return Average mean squared logarithmic error (aMSLE)
#' @details aMSLE for survey # => mean value of the MSLEs for specified variables in
#' survey # => mean value of MSLEs for objects in Observed#=data.frame()
#' @examples AVEMSLE(Actuals1=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed1=data.frame(TESTNUMB$O1Q1, TESTNUMB$O1Q2),
#' Actuals2=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed2=data.frame(TESTNUMB$O2Q1, TESTNUMB$O2Q2),
#' Actuals3=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed3=data.frame(TESTNUMB$O3Q1, TESTNUMB$O3Q2))
#' @note Make sure to properly order inputs, per the example: Actuals1=data.frame()
#' objects and corrusponding Observed1=data.frame() objects must be given in the
#' same order as each other; and ... must be given in numbered pairs of Actuals#,
#' Observed#, and those pairs given in sequence of their #s.
#' @export AVEMSLE
AVEMSLE=function(Actuals1=data.frame(), Observed1=data.frame(), ...){
  #create lists from ...
    other=list(...)
    otherActuals=cbind(other[grepl("Actuals",names(other))])
    otherObserved=cbind(other[grepl("Observed",names(other))])
  #create storage vector
    avemslevec=vector(length=(length(otherObserved)+1))
  #calculate and store data set 1 values
    avemsle=mean(apply(((log(Actuals1+1)-log(Observed1+1))^2), 2, mean))
    avemslevec[1]=avemsle
  #calculate and store ... values
    for(i in 1:length(otherObserved)){
      avemslemore=mean(apply(((log(otherActuals[[i]]+1)-
        log(otherObserved[[i]]+1))^2), 2, mean))
      avemslevec[i+1]=avemslemore
  }
  #convert to matrix and label
    avemslematrix=noquote(matrix(cbind(format(avemslevec, signif=7))))
    rownames(avemslematrix)=paste("   survey",
      seq(along=avemslematrix), " => ")
    colnames(avemslematrix)=c("aMSLE")
  #return results
    avemslematrix
}

#' @title Calculate average root mean squared logarithmic error (aRMSLE)
#' @description Calculates average root mean squared logarithmic error (aRMSLE) for
#' one or more surveys
#' @param Actuals1 = data from a "gold standard" survey; objects are variable columns
#' from "gold standard" survey that corruspond to variable columns Observed1
#' @param Observed1 = data from survey 1; objects are variable columns from
#' survey 1 that corruspond to variable columns from Actuals1
#' @param ... = "gold standard" data/survey # data for additional surveys
#' @return Average root mean squared logarithmic error (aRMSLE)
#' @details aRMSLE for survey # => mean value of the RMSLEs for specified variables in
#' survey # => mean value of RMSLEs for objects in Observed#=data.frame()
#' @examples AVERMSLE(Actuals1=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed1=data.frame(TESTNUMB$O1Q1, TESTNUMB$O1Q2),
#' Actuals2=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed2=data.frame(TESTNUMB$O2Q1, TESTNUMB$O2Q2),
#' Actuals3=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed3=data.frame(TESTNUMB$O3Q1, TESTNUMB$O3Q2))
#' @note Make sure to properly order inputs, per the example: Actuals1=data.frame()
#' objects and corrusponding Observed1=data.frame() objects must be given in the
#' same order as each other; and ... must be given in numbered pairs of Actuals#,
#' Observed#, and those pairs given in sequence of their #s.
#' @export AVERMSLE
AVERMSLE=function(Actuals1=data.frame(), Observed1=data.frame(), ...){
  #create lists from ...
    other=list(...)
    otherActuals=cbind(other[grepl("Actuals",names(other))])
    otherObserved=cbind(other[grepl("Observed",names(other))])
  #create storage vector
    avermslevec=vector(length=(length(otherObserved)+1))
  #calculate and store data set 1 values
    avermsle=mean(sqrt(apply(((log(Actuals1+1)-log(Observed1+1))^2), 2, mean)))
    avermslevec[1]=avermsle
  #calculate and store ... values
    for(i in 1:length(otherObserved)){
      avermslemore=mean(sqrt(apply(((log(otherActuals[[i]]+1)-
        log(otherObserved[[i]]+1))^2), 2, mean)))
      avermslevec[i+1]=avermslemore
    }
  #convert to matrix and label
    avermslematrix=noquote(matrix(cbind(format(avermslevec, signif=7))))
    rownames(avermslematrix)=paste("   survey",
      seq(along=avermslematrix), " => ")
    colnames(avermslematrix)=c("aRMSLE")
  #print results
    avermslematrix
}

#' @title Calculate full scale-dependent statistics
#' @description Calculates full scale-dependent statistics for one or more surveys
#' @param Actuals1 = data from a "gold standard" survey; objects are variable columns
#' from "gold standard" survey that corruspond to variable columns Observed1
#' @param Observed1 = data from survey 1; objects are variable columns from
#' survey 1 that corruspond to variable columns from Actuals1
#' @param ... = "gold standard" data/survey # data for additional surveys
#' @return Full scale-dependent statistics
#' @examples FULLSD(Actuals1=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed1=data.frame(TESTNUMB$O1Q1, TESTNUMB$O1Q2),
#' Actuals2=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed2=data.frame(TESTNUMB$O2Q1, TESTNUMB$O2Q2),
#' Actuals3=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed3=data.frame(TESTNUMB$O3Q1, TESTNUMB$O3Q2))
#' @note Make sure to properly order inputs, per the example: Actuals1=data.frame()
#' objects and corrusponding Observed1=data.frame() objects must be given in the
#' same order as each other; and ... must be given in numbered pairs of Actuals#,
#' Observed#, and those pairs given in sequence of their #s.
#' @export FULLSD
FULLSD=function(Actuals1=data.frame(), Observed1=data.frame(), ...){
  #create lists from ...
    other=list(...)
    otherActuals=cbind(other[grepl("Actuals",names(other))])
    otherObserved=cbind(other[grepl("Observed",names(other))])
  #create storage vectors
    avemaevec=vector(length=(length(otherObserved)+1))
    avemsevec=vector(length=(length(otherObserved)+1))
    avermsevec=vector(length=(length(otherObserved)+1))
    avemslevec=vector(length=(length(otherObserved)+1))
    avermslevec=vector(length=(length(otherObserved)+1))
    spacevec=vector(length=(length(otherObserved)+1))
    spacevec[1:(length(otherObserved)+1)]=" "
    #calculate and store data set 1 values
    avemae=mean(apply(abs(Actuals1-Observed1), 2, mean))
    avemaevec[1]=avemae
    avemse=mean(apply(((Actuals1-Observed1)^2), 2, mean))
    avemsevec[1]=avemse
    avermse=mean(sqrt(apply(((Actuals1-Observed1)^2), 2, mean)))
    avermsevec[1]=avermse
    avemsle=mean(apply(((log(Actuals1+1)-
      log(Observed1+1))^2), 2, mean))
    avemslevec[1]=avemsle
    avermsle=mean(sqrt(apply(((log(Actuals1+1)-
      log(Observed1+1))^2), 2, mean)))
    avermslevec[1]=avermsle
  #calculate and store ... values
    for(i in 1:length(otherObserved)){
      avemaemore=mean(apply(abs(otherActuals[[i]]-
        otherObserved[[i]]), 2, mean))
      avemaevec[i+1]=avemaemore
      avemsemore=mean(apply(((otherActuals[[i]]-
        otherObserved[[i]])^2), 2, mean))
      avemsevec[i+1]=avemsemore
      avermsemore=mean(sqrt(apply(((otherActuals[[i]]-
        otherObserved[[i]])^2), 2, mean)))
      avermsevec[i+1]=avermsemore
      avemslemore=mean(apply(((log(otherActuals[[i]]+1)-
        log(otherObserved[[i]]+1))^2), 2, mean))
      avemslevec[i+1]=avemslemore
      avermslemore=mean(sqrt(apply(((log(otherActuals[[i]]+1)-
        log(otherObserved[[i]]+1))^2), 2, mean)))
      avermslevec[i+1]=avermslemore
    }
  #convert to matrix and label
    fullsdmatrix=noquote(matrix(cbind(format(avemaevec, signif=7), spacevec,
      format(avemsevec, signif=7), spacevec, format(avermsevec, signif=7),
      spacevec, format(avemslevec, signif=7), spacevec,
      format(avermslevec, signif=7)), ncol=9))
    rownames(fullsdmatrix)=paste("   survey",
      seq_along(fullsdmatrix[, 1]), " => ")
    colnames(fullsdmatrix)=c("aMAE", " ", "aMSE", " ", "aRMSE", " ",
      "aMSLE", " ", "aRMSLE")
  #return results
    fullsdmatrix
}

#' @title Calculate average mean absolute percentage error (aMAPE)
#' @description Calculates average mean absolute percentage error (aMAPE) for one
#' or more surveys
#' @param Actuals1 = data from a "gold standard" survey; objects are variable columns
#' from "gold standard" survey that corruspond to variable columns Observed1
#' @param Observed1 = data from survey 1; objects are variable columns from
#' survey 1 that corruspond to variable columns from Actuals1
#' @param ... = "gold standard" data/survey # data for additional surveys
#' @return Average mean absolute percentage error (aMAPE)
#' @details aMAPE for survey # => mean value of the MAPEs for specified variables in
#' survey # => mean value of MAPEs for objects in Observed#=data.frame()
#' @examples AVEMAPE(Actuals1=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed1=data.frame(TESTNUMB$O1Q1, TESTNUMB$O1Q2),
#' Actuals2=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed2=data.frame(TESTNUMB$O2Q1, TESTNUMB$O2Q2),
#' Actuals3=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed3=data.frame(TESTNUMB$O3Q1, TESTNUMB$O3Q2))
#' @note Make sure to properly order inputs, per the example: Actuals1=data.frame()
#' objects and corrusponding Observed1=data.frame() objects must be given in the
#' same order as each other; and ... must be given in numbered pairs of Actuals#,
#' Observed#, and those pairs given in sequence of their #s.
#' @export AVEMAPE
AVEMAPE=function(Actuals1=data.frame(), Observed1=data.frame(), ...){
  #create lists from ...
    other=list(...)
    otherActuals=cbind(other[grepl("Actuals",names(other))])
    otherObserved=cbind(other[grepl("Observed",names(other))])
  #create storage vector
    avemapevec=vector(length=(length(otherObserved)+1))
  #calculate and store data set 1 values
    avemape=mean(apply(abs(Actuals1-Observed1)/Actuals1, 2, mean))
    avemapevec[1]=avemape
  #calculate and store ... values
    for(i in 1:length(otherObserved)){
      avemapemore=mean(apply(abs(otherActuals[[i]]-
        otherObserved[[i]])/otherActuals[[i]], 2, mean))
      avemapevec[i+1]=avemapemore
    }
  #convert to matrix and label
    avemapematrix=noquote(matrix(cbind(format(avemapevec, signif=7))))
    rownames(avemapematrix)=paste("   survey",
      seq(along=avemapematrix), " => ")
    colnames(avemapematrix)=c("aMAPE")
  #return results
    avemapematrix
}

#' @title Calculate average symmetric mean absolute percentage error (aSMAPE)
#' @description Calculates average symmetric mean absolute percentage error (aSMAPE)
#' for one or more surveys
#' @param Actuals1 = data from a "gold standard" survey; objects are variable columns
#' from "gold standard" survey that corruspond to variable columns Observed1
#' @param Observed1 = data from survey 1; objects are variable columns from
#' survey 1 that corruspond to variable columns from Actuals1
#' @param ... = "gold standard" data/survey # data for additional surveys
#' @return Average symmetric mean absolute percentage error (aSMAPE)
#' @details aSMAPE for survey # => mean value of the SMAPEs for specified variables in
#' survey # => mean value of SMAPEs for objects in Observed#=data.frame()
#' @examples AVESMAPE(Actuals1=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed1=data.frame(TESTNUMB$O1Q1, TESTNUMB$O1Q2),
#' Actuals2=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed2=data.frame(TESTNUMB$O2Q1, TESTNUMB$O2Q2),
#' Actuals3=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed3=data.frame(TESTNUMB$O3Q1, TESTNUMB$O3Q2))
#' @note Make sure to properly order inputs, per the example: Actuals1=data.frame()
#' objects and corrusponding Observed1=data.frame() objects must be given in the
#' same order as each other; and ... must be given in numbered pairs of Actuals#,
#' Observed#, and those pairs given in sequence of their #s.
#' @export AVESMAPE
AVESMAPE=function(Actuals1=data.frame(), Observed1=data.frame(), ...){
  #create lists from ...
    other=list(...)
    otherActuals=cbind(other[grepl("Actuals",names(other))])
    otherObserved=cbind(other[grepl("Observed",names(other))])
  #create storage vector
    avesmapevec=vector(length=(length(otherObserved)+1))
  #calculate and store data set 1 values
    avesmape=mean((apply((abs(Actuals1-Observed1)/
      (abs(Actuals1)+abs(Observed1))), 2, mean))*2)
    avesmapevec[1]=avesmape
  #calculate and store ... values
    for(i in 1:length(otherObserved)){
      avesmapemore=mean((apply((abs(otherActuals[[i]]-
        otherObserved[[i]])/(abs(otherActuals[[i]])+
        abs(otherObserved[[i]]))), 2, mean))*2)
      avesmapevec[i+1]=avesmapemore
    }
  #convert to matrix and label
    avesmapematrix=noquote(matrix(cbind(format(avesmapevec, signif=7))))
    rownames(avesmapematrix)=paste("   survey",
      seq(along=avesmapematrix), " => ")
    colnames(avesmapematrix)=c("aSMAPE")
  #return results
    avesmapematrix
}

#' @title Calculate average relative absolute error (aRAE)
#' @description Calculates average relative absolute error (aRAE) for one
#' or more surveys
#' @param Actuals1 = data from a "gold standard" survey; objects are variable columns
#' from "gold standard" survey that corruspond to variable columns Observed1
#' @param Observed1 = data from survey 1; objects are variable columns from
#' survey 1 that corruspond to variable columns from Actuals1
#' @param ... = "gold standard" data/survey # data for additional surveys
#' @return Average relative absolute error (aRAE)
#' @details aRAE for survey # => mean value of the RAEs for specified variables in
#' survey # => mean value of RAEs for objects in Observed#=data.frame()
#' @examples AVERAE(Actuals1=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed1=data.frame(TESTNUMB$O1Q1, TESTNUMB$O1Q2),
#' Actuals2=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed2=data.frame(TESTNUMB$O2Q1, TESTNUMB$O2Q2),
#' Actuals3=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed3=data.frame(TESTNUMB$O3Q1, TESTNUMB$O3Q2))
#' @note Make sure to properly order inputs, per the example: Actuals1=data.frame()
#' objects and corrusponding Observed1=data.frame() objects must be given in the
#' same order as each other; and ... must be given in numbered pairs of Actuals#,
#' Observed#, and those pairs given in sequence of their #s.
#' @export AVERAE
AVERAE=function(Actuals1=data.frame(), Observed1=data.frame(), ...){
  #create lists from ...
    other=list(...)
    otherActuals=cbind(other[grepl("Actuals",names(other))])
    otherObserved=cbind(other[grepl("Observed",names(other))])
  #create storage vector
    averaevec=vector(length=(length(otherObserved)+1))
  #calculate and store data set 1 values
    averae=mean((apply(abs(Actuals1-Observed1), 2, sum))/
      apply((abs(mapply('-', Actuals1, apply(Actuals1, 2, mean)))), 2, sum))
    averaevec[1]=averae
  #calculate and store ... values
    for(i in 1:length(otherObserved)){
      averaemore=mean((apply(abs(otherActuals[[i]]-
        otherObserved[[i]]), 2, sum))/apply((abs(mapply('-',
        otherActuals[[i]], apply(otherActuals[[i]], 2,
        mean)))), 2, sum))
      averaevec[i+1]=averaemore
    }
  #convert to matrix and label
    averaematrix=noquote(matrix(cbind(format(averaevec, signif=7))))
    rownames(averaematrix)=paste("   survey",
      seq(along=averaematrix), " => ")
    colnames(averaematrix)=c("aRAE")
  #return results
    averaematrix
}

#' @title Calculate average relative squared error (aRSE)
#' @description Calculates average relative squared error (aRSE) for one
#' or more surveys
#' @param Actuals1 = data from a "gold standard" survey; objects are variable columns
#' from "gold standard" survey that corruspond to variable columns Observed1
#' @param Observed1 = data from survey 1; objects are variable columns from
#' survey 1 that corruspond to variable columns from Actuals1
#' @param ... = "gold standard" data/survey # data for additional surveys
#' @return Average relative squared error (aRSE)
#' @details aRSE for survey # => mean value of the RSEs for specified variables in
#' survey # => mean value of RSEs for objects in Observed#=data.frame()
#' @examples AVERSE(Actuals1=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed1=data.frame(TESTNUMB$O1Q1, TESTNUMB$O1Q2),
#' Actuals2=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed2=data.frame(TESTNUMB$O2Q1, TESTNUMB$O2Q2),
#' Actuals3=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed3=data.frame(TESTNUMB$O3Q1, TESTNUMB$O3Q2))
#' @note Make sure to properly order inputs, per the example: Actuals1=data.frame()
#' objects and corrusponding Observed1=data.frame() objects must be given in the
#' same order as each other; and ... must be given in numbered pairs of Actuals#,
#' Observed#, and those pairs given in sequence of their #s.
#' @export AVERSE
AVERSE=function(Actuals1=data.frame(), Observed1=data.frame(), ...){
  #create lists from ...
    other=list(...)
    otherActuals=cbind(other[grepl("Actuals",names(other))])
    otherObserved=cbind(other[grepl("Observed",names(other))])
  #create storage vector
    aversevec=vector(length=(length(otherObserved)+1))
  #calculate and store data set 1 values
    averse=mean((apply((Actuals1-Observed1)^2, 2, sum))/
      apply(((mapply('-', Actuals1, apply(Actuals1, 2, mean)))^2), 2, sum))
    aversevec[1]=averse
  #calculate and store ... values
    for(i in 1:length(otherObserved)){
      aversemore=mean((apply((otherActuals[[i]]-
        otherObserved[[i]])^2, 2, sum))/apply(((mapply('-',
        otherActuals[[i]], apply(otherActuals[[i]], 2,
        mean)))^2), 2, sum))
      aversevec[i+1]=aversemore
    }
  #convert to matrix and label
    aversematrix=noquote(matrix(cbind(format(aversevec, signif=7))))
    rownames(aversematrix)=paste("   survey",
      seq(along=aversematrix), " => ")
    colnames(aversematrix)=c("aRSE")
  #return results
    aversematrix
}

#' @title Calculate average root relative squared error (aRRSE)
#' @description Calculates average root relative squared error (aRRSE) for one
#' or more surveys
#' @param Actuals1 = data from a "gold standard" survey; objects are variable columns
#' from "gold standard" survey that corruspond to variable columns Observed1
#' @param Observed1 = data from survey 1; objects are variable columns from
#' survey 1 that corruspond to variable columns from Actuals1
#' @param ... = "gold standard" data/survey # data for additional surveys
#' @return Average root relative squared error (aRRSE)
#' @details aRRSE for survey # => mean value of the RRSEs for specified variables in
#' survey # => mean value of RRSEs for objects in Observed#=data.frame()
#' @examples AVERRSE(Actuals1=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed1=data.frame(TESTNUMB$O1Q1, TESTNUMB$O1Q2),
#' Actuals2=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed2=data.frame(TESTNUMB$O2Q1, TESTNUMB$O2Q2),
#' Actuals3=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed3=data.frame(TESTNUMB$O3Q1, TESTNUMB$O3Q2))
#' @note Make sure to properly order inputs, per the example: Actuals1=data.frame()
#' objects and corrusponding Observed1=data.frame() objects must be given in the
#' same order as each other; and ... must be given in numbered pairs of Actuals#,
#' Observed#, and those pairs given in sequence of their #s.
#' @export AVERRSE
AVERRSE=function(Actuals1=data.frame(), Observed1=data.frame(), ...){
  #create lists from ...
    other=list(...)
    otherActuals=cbind(other[grepl("Actuals",names(other))])
    otherObserved=cbind(other[grepl("Observed",names(other))])
  #create storage vector
    averrsevec=vector(length=(length(otherObserved)+1))
  #calculate and store data set 1 values
    averrse=mean(sqrt((apply((Actuals1-Observed1)^2, 2, sum))/
      apply(((mapply('-', Actuals1, apply(Actuals1, 2, mean)))^2), 2, sum)))
    averrsevec[1]=averrse
  #calculate and store ... values
    for(i in 1:length(otherObserved)){
      averrsemore=mean(sqrt((apply((otherActuals[[i]]-
        otherObserved[[i]])^2, 2, sum))/apply(((mapply('-',
        otherActuals[[i]], apply(otherActuals[[i]], 2,
        mean)))^2), 2, sum)))
      averrsevec[i+1]=averrsemore
    }
  #convert to matrix and label
    averrsematrix=noquote(matrix(cbind(format(averrsevec, signif=7))))
    rownames(averrsematrix)=paste("   survey",
      seq(along=averrsematrix), " => ")
    colnames(averrsematrix)=c("aRRSE")
  #return results
    averrsematrix
}

#' @title Calculate full scale-independent statistics
#' @description Calculates full scale-independent statistics for one or more surveys
#' @param Actuals1 = data from a "gold standard" survey; objects are variable columns
#' from "gold standard" survey that corruspond to variable columns Observed1
#' @param Observed1 = data from survey 1; objects are variable columns from
#' survey 1 that corruspond to variable columns from Actuals1
#' @param ... = "gold standard" data/survey # data for additional surveys
#' @return Full scale-independent statistics
#' @examples FULLSI(Actuals1=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed1=data.frame(TESTNUMB$O1Q1, TESTNUMB$O1Q2),
#' Actuals2=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed2=data.frame(TESTNUMB$O2Q1, TESTNUMB$O2Q2),
#' Actuals3=data.frame(TESTNUMB$AQ1, TESTNUMB$AQ2),
#' Observed3=data.frame(TESTNUMB$O3Q1, TESTNUMB$O3Q2))
#' @note Make sure to properly order inputs, per the example: Actuals1=data.frame()
#' objects and corrusponding Observed1=data.frame() objects must be given in the
#' same order as each other; and ... must be given in numbered pairs of Actuals#,
#' Observed#, and those pairs given in sequence of their #s.
#' @export FULLSI
FULLSI=function(Actuals1=data.frame(), Observed1=data.frame(), ...){
  #create lists from ...
    other=list(...)
    otherActuals=cbind(other[grepl("Actuals",names(other))])
    otherObserved=cbind(other[grepl("Observed",names(other))])
  #create storage vectors
    avemapevec=vector(length=(length(otherObserved)+1))
    avesmapevec=vector(length=(length(otherObserved)+1))
    averaevec=vector(length=(length(otherObserved)+1))
    aversevec=vector(length=(length(otherObserved)+1))
    averrsevec=vector(length=(length(otherObserved)+1))
    spacevec=vector(length=(length(otherObserved)+1))
    spacevec[1:(length(otherObserved)+1)]=" "
  #calculate and store data set 1 values
    avemape=mean(apply(abs(Actuals1-Observed1)/Actuals1, 2, mean))
    avemapevec[1]=avemape
    avesmape=mean((apply((abs(Actuals1-Observed1)/
      (abs(Actuals1)+abs(Observed1))), 2, mean))*2)
    avesmapevec[1]=avesmape
    averae=mean((apply(abs(Actuals1-Observed1), 2, sum))/
      apply((abs(mapply('-', Actuals1, apply(Actuals1, 2,
      mean)))), 2, sum))
    averaevec[1]=averae
    averse=mean((apply((Actuals1-Observed1)^2, 2, sum))/
      apply(((mapply('-', Actuals1, apply(Actuals1, 2,
      mean)))^2), 2, sum))
    aversevec[1]=averse
    averrse=mean(sqrt((apply((Actuals1-Observed1)^2, 2, sum))/
      apply(((mapply('-', Actuals1, apply(Actuals1, 2,
      mean)))^2), 2, sum)))
    averrsevec[1]=averrse
  #calculate and store ... values
    for(i in 1:length(otherObserved)){
      avemapemore=mean(apply(abs(otherActuals[[i]]-
        otherObserved[[i]])/otherActuals[[i]], 2, mean))
      avemapevec[i+1]=avemapemore
      avesmapemore=mean((apply((abs(otherActuals[[i]]-
        otherObserved[[i]])/(abs(otherActuals[[i]])+
        abs(otherObserved[[i]]))), 2, mean))*2)
      avesmapevec[i+1]=avesmapemore
      averaemore=mean((apply(abs(otherActuals[[i]]-
        otherObserved[[i]]), 2, sum))/apply((abs(mapply('-',
        otherActuals[[i]], apply(otherActuals[[i]], 2,
        mean)))), 2, sum))
      averaevec[i+1]=averaemore
      aversemore=mean((apply((otherActuals[[i]]-
        otherObserved[[i]])^2, 2, sum))/apply(((mapply('-',
        otherActuals[[i]], apply(otherActuals[[i]], 2,
        mean)))^2), 2, sum))
      aversevec[i+1]=aversemore
      averrsemore=mean(sqrt((apply((otherActuals[[i]]-
        otherObserved[[i]])^2, 2, sum))/apply(((mapply('-',
        otherActuals[[i]], apply(otherActuals[[i]], 2,
        mean)))^2), 2, sum)))
      averrsevec[i+1]=averrsemore
    }
  #convert to matrix and label
    fullsimatrix=noquote(matrix(cbind(format(avemapevec, signif=7), spacevec,
      format(avesmapevec, signif=7), spacevec, format(averaevec, signif=7),
      spacevec, format(aversevec, signif=7), spacevec, format(averrsevec,
      signif=7)), ncol=9))
    rownames(fullsimatrix)=paste("   survey",
      seq_along(fullsimatrix[, 1]), " => ")
    colnames(fullsimatrix)=c("aMAPE", " ", "aSMAPE", " ", "aRAE", " ",
      "aRSE", " ", "aRRSE")
  #return results
    fullsimatrix
}
