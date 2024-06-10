
d1a = read.csv("tblPricing Final Extract For ERA One 2016-17 (1) uwa.csv");
dim(d1a[, ])

d1b = read.csv("tblPricing Final Extract For ERA Two 2016-17 (2) UWA.csv");
dim(d1b)

d2a = read.csv("tblPricing Final Extract For ERA One 2017-18 (1) uwa.csv");
dim(d2a)
d2b = read.csv("tblPricing Final Extract For ERA Two 2017-18 (2) UWA.csv");
dim(d2b)

d3a = read.csv("tblPricing Final Extract For ERA One 2018-19 (1) uwa.csv");
dim(d3a)
d3b = read.csv("tblPricing Final Extract For ERA Two 2018-19 (2) UWA.csv");
dim(d3b)

d4a = read.csv("tblPricing Final Extract For ERA One 2019-20 (1) uwa.csv");
dim(d4a)
d4b = read.csv("tblPricing Final Extract For ERA Two 2019-20 (2) UWA.csv");
dim(d4b)

options(scipen=999)

#process d*a files first to check

blknames = c("ReadDate","WaterUse","WtrUseYr","CycleNum");

#reshape data for data file d1a (2015/2016)
d1a.names = d1a
colnames(d1a.names) = c(names(d1a)[1:115], paste(blknames,
                                                 c(rep(4:6,each=4),
                                                   rep(1:6,each=4),
                                                   rep(1:6,each=4)),
                                                 sep="."));
head(d1a.names)
d1a.long = reshape(d1a.names[,1:139], direction = "long",
                   varying = names(d1a.names)[116:139], sep = ".")
d1a.long$data.file = "d1a"
d1a.long = d1a.long[order(d1a.long$Account.Number), ]
write.csv(d1a.long, file="d1a.long")
head(d1a.long[,
              c(1,2,116:120)], n=43)


#data file d2a
d2a.names = d2a
colnames(d2a.names) = c(names(d2a)[1:115], paste(blknames,
                                                 c(rep(4:6,each=4),
                                                   rep(1:6,each=4),
                                                   rep(1:6,each=4)),
                                                 sep="."));
head(d2a.names)
d2a.long = reshape(d2a.names[,1:139], direction = "long",
                   varying = names(d2a.names)[116:139], sep = ".")
d2a.long$data.file = "d2a";
d2a.long = d2a.long[order(d2a.long$Account.Number), ]
#write.csv(d2a.long, file="d2a.long")
head(d2a.long[,c(1,2,116:120)],n=40)


#data file d3a
d3a.names = d3a
colnames(d3a.names) = c(names(d3a)[1:115], paste(blknames,
                                                 c(rep(4:6,each=4),
                                                   rep(1:6,each=4),
                                                   rep(1:6,each=4)),
                                                 sep="."));
head(d3a.names)
d3a.long = reshape(d3a.names[,1:139], direction = "long",
                   varying = names(d3a.names)[116:139], sep = ".")
d3a.long$data.file = "d3a";
d3a.long = d3a.long[order(d3a.long$Account.Number), ]
#write.csv(d3a.long, file="d3a.long")
head(d3a.long[,c(1,2,116:120)],n=40)


#data file d4a
d4a.names = d4a[,1:175]
colnames(d4a.names) = c(names(d4a)[1:115], paste(blknames,
                                                 c(rep(4:6,each=4),
                                                   rep(1:3,each=4),
                                                   rep(10004:10006,each=4),
                                                   rep(20001:20006,each=4)),
                                                 sep="."));
head(d4a.names)
d4a.long = reshape(d4a.names[,1:175], direction = "long",
                   varying = names(d4a.names)[116:175], sep = ".")
d4a.long$data.file = "d4a";

d4a.long[d4a.long$time > 10000 & d4a.long$time < 20000, "time"] =
  d4a.long[d4a.long$time > 10000 & d4a.long$time < 20000, "time"] - 10000;
d4a.long[d4a.long$time > 20000, "time"] =
  d4a.long[d4a.long$time > 20000, "time"] - 20000;
d4a.long = d4a.long[order(d4a.long$Account.Number), ]

#write.csv(d4a.long, file="d4a.long")
head(d4a.long[,c(1,2,116:120)],n=42)

#save memory space
rm(d1a,d2a,d3a,d4a);
save(d1a.long,file="d1a.long");
save(d2a.long,file="d2a.long");
save(d3a.long,file="d3a.long");
save(d4a.long,file="d4a.long");

load("d1a.long")
load("d2a.long")
load("d3a.long")
load("d4a.long")

das = rbind(d1a.long,d2a.long,d3a.long,d4a.long)
rm(d1a.long, d2a.long, d3a.long, d4a.long)

#das[das$Account.Number=="9007999997", c(1,2,116:122)]
das = das[order(das$Account.Number), ]
save(das, file="das.RDATA")

tail(das[2001:3000,c(1,2,116:122)],n=85);

#water price data
prices = read.csv("Prices.csv")

#prices for
yr=2016
prices[,yr-2014]
#prices for
yr=2020
prices[,yr-2014]

#determine marginal cost and cost of water use for period
getcost = function(wsum,prices,year){
  pvec = prices[1:3,year-2014]
  if(is.na(wsum)){
    list(cost=NA,mp=NA);
  } else {
    #given prices for blocks (in pvec), calculate cost of q
    if(wsum <= 150){
      mp = pvec[1];
      cost = wsum*mp;
    } else if((wsum > 150) & (wsum <= 500)){
      mp = pvec[2];
      cost = 150*pvec[1] + (wsum-150)*mp;
    } else {#above 500
      mp = pvec[3];
      cost = 150*pvec[1] + 350*pvec[2] + (wsum-500)*mp;
    }
    list(cost=cost,mp=mp);
  }
}


#function for assembling column names
getcolname = function(prefix,y,pr){
  paste(prefix,y,".period.",
        pr,months[pr],".Water.Use",sep="");
}

getBlock = function(num,d=NULL, len=33) {
  lwr = (num-1)*len + 1;
  upr = lwr + len - 1;
  res = NULL;
  if(is.null(d)){
    res = lwr:upr;
  } else {
    res = d[lwr:upr,]
  }
  res;
}

#calc water sum within cycles
calcwsum = function(blki, cyclsi){
  for(rowi in cyclsi[1]:dim(blki)[1]){

    if(rowi %in% cyclsi){
      blki[rowi,"WaterSum"] = blki[rowi,"WaterUse"];
      cost.n.mp = getcost(blki[rowi,"WaterSum"],prices,blki[rowi,"WtrUseYr"]);
      blki[rowi,"Cost"] =  cost.n.mp[[1]];
      blki[rowi,"MP"] =  cost.n.mp[[2]];
      blki[rowi,"CostInclusive"] = cost.n.mp[[1]];

    } else {
      blki[rowi,"WaterSum"] = blki[rowi-1,"WaterSum"] +
                              blki[rowi,  "WaterUse"];
      cost.n.mp = getcost(blki[rowi,"WaterSum"],prices,blki[rowi,"WtrUseYr"]);
      blki[rowi,"MP"] =  cost.n.mp[[2]];
      blki[rowi,"CostInclusive"] = cost.n.mp[[1]];
      blki[rowi,"Cost"] =  blki[rowi,"CostInclusive"] - blki[rowi-1,"CostInclusive"];
    }
    blki[rowi,"AP"] = blki[rowi,"Cost"]/blki[rowi,"WaterUse"];
    blki[rowi,"APInclusive"] = blki[rowi,"CostInclusive"]/blki[rowi,"WaterSum"];
  }
  blki;
}


load("das.RDATA")
dim(das)

#add columns for cost and prices
das$WaterSum = NA;
das$Cost = NA;
das$CostInclusive = NA;
das$APInclusive = NA;
das$AP = NA;
das$MP = NA;
names(das)

#uac = unique(das$Account.Number)
#length(uac)

uniqueAcctNo = dim(das)[1]/33;

watercols = c("WaterUse","WtrUseYr","CycleNum","id","data.file",
"WaterSum","Cost","CostInclusive","APInclusive","AP","MP");
watercols.2 = c("WaterSum","Cost","CostInclusive","APInclusive","AP","MP");

d1 = date()
for(accti in 101:1000){#loop through accounts
  print(accti);
 #get rows
 rws=getBlock(accti);
 print(rws);
 #get data
 blk=getBlock(accti,das);
 #identify where new cycles start
 if(!is.na(min(blk$CycleNum))){
   if(min(blk$CycleNum) == 1){
     #for the usual cycles that go 1 to 6
     cyclestarts = which(blk$CycleNum == 1);
   } else if(min(blk$CycleNum) == 2){
     #for cycles that go 2, 4, ..,12
     cyclestarts = which(blk$CycleNum == 2);
   }
   print(cyclestarts)

   #calculate water cumulative sums
   blkupdated = calcwsum(blk[,watercols], cyclestarts);
   das[rws,watercols.2] = blkupdated[,watercols.2];
 }
}#end of account loop
d2 = date()
d1
d2

#write.csv(das[1:33000,],file="das_1000_accounts.csv",row.names=F)
write.csv(das[1:33000,c(1,2,13,26,116:128)],file="das_1000_accounts_b.csv",row.names=F)

###########################
