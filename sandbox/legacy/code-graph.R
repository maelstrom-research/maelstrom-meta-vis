# read table from J Bergeron
# REMOVE all dashes in names of either constructs or studies
 
toto<-read.table(file="C:\\Users\\ghislain\\Documents\\cognitive-scales-IALSA\\Processing speed - coverage.txt",
                 header=T,sep="\t")

# Exclude constructs with only one study 
toto1<-subset(toto, select = c(Study,Choice.Reaction.Time,Pattern.comparison,Simple.Reaction.Time,
                             Symbol.Digit.Modalities.Test,WAIS.Digit.Symbol.test) )

# NB: Automatic selection will have to be coded

library(sand)

# Within each construct, generate edges between studies

# For Choice.Reaction.Time, edges are:
CRT.g<-graph.full(5)
V(CRT.g)$name<-c("ACTIVE","CaPS","CLS","OBAS","TILDA")
plot(CRT.g)

# For Pattern.comparison, edges are:
PC.g<-graph.full(2)
V(PC.g)$name<-c("LBLS","SNAC.K")
plot(PC.g)

# For Simple.Reaction.Time, edges are:
SRT.g<-graph.full(2)
V(SRT.g)$name<-c("CLS","OBAS")
plot(SRT.g)

# For Symbol.Digit.Modalities.Test, edges are:
SDMT.g<-graph.full(7)
V(SDMT.g)$name<-c("ACTIVE","ALSA","SydneyMAS","OBAS","OCTO.Twin","PATH","SATSA")
plot(SDMT.g)

# For WAIS.Digit.Symbol.test, edges are:
WDST.g<-graph.full(3)
V(WDST.g)$name<-c("CLS","DEAS","OBAS")
plot(WDST.g)

# create a single graph with edges having different weights
titi<-rbind( get.edgelist(CRT.g),
       get.edgelist(PC.g),
       get.edgelist(SRT.g),
       get.edgelist(SDMT.g),
       get.edgelist(WDST.g) )
titi<-graph(t(titi),directed=F)
E(titi)$weight<-1      # set weight=1
titi<-simplify(titi)   # convert multi-graph to weighted simple graph
E(titi)$weight
E(titi)$color<-E(titi)$weight
E(titi)$width<-E(titi)$weight
V(titi)$color<-"yellow"

plot(titi,edge.width=E(titi)$width,edge.color=E(titi)$color,
                     layout=layout.kamada.kawai)

# create a bipartite graph with constructs as members of studies
titi.bip<-graph.formula( CRT:PC:SRT:SDMT:WDST,
         ACTIVE:ALSA:CaPS:CLS:DEAS:LBLS:SydneyMAS:OBAS:OCTO.Twin:PATH:SATSA:SNAC.K:TILDA,
         ACTIVE:CaPS:CLS:OBAS:TILDA - CRT,
            LBLS:SNAC.K - PC, CLS:OBAS - SRT, 
               ACTIVE:ALSA:SydneyMAS:OBAS:OCTO.Twin:PATH:SATSA - SDMT,
                 CLS:DEAS:OBAS - WDST )
V(titi.bip)$type<-as.logical(c(rep(1,5),rep(0,13)))
V(titi.bip)$size<-8*ifelse(V(titi.bip)$type,graph.strength(titi.bip),2)
plot(titi.bip, vertex.color=ifelse(V(titi.bip)$type,"cyan","yellow"),
               layout=layout.kamada.kawai )
plot(titi.bip, vertex.shape=ifelse(V(titi.bip)$type,"circle","circle"),
               vertex.color=ifelse(V(titi.bip)$type,"cyan","yellow"),
               layout=layout.bipartite(titi.bip)[,2:1] )


###############################################################
# new analysis for memory scale
# read table of Memory scale-coverage I have generated from MR search 
# REMOVE all dashes in names of either constructs or studies
##############################################################
 
memory<-read.csv(file="C:\\Users\\ghislain\\Documents\\cognitive-scales-IALSA\\Memory scale-coverage.csv",
                 header=T,sep=",",na.strings="")
memory<-memory[,-c(2,3)]
which(apply(memory[,-1]!=0,2,sum)==1)    # to identify constructs used by only 1 study

# Exclude 1,2,4,7,8,9,14,15,16,17,18,22,24,25,26,28,30,31,33,34,39
memory1<-memory[,-c(2,3,5,8,9,10,15,16,17,18,19,23,25,26,27,29,31,32,34,35,40)] 
which(apply(memory1[,-1]!=0,2,sum)==1)   # OK 21 constructs remain
which(apply(memory1[,-1]!=0,1,sum)==0)   # exclude study 3 (ALSA) and 16 (KOCOA) with no constructs                                                                  
                                                                      
library(sand)

### Within each construct, generate edges between studies ###

# For Benton.Visual.Retention.Test, edges are:
BVRT.g<-graph.full(4)
V(BVRT.g)$name<-c("CSHA","SydneyMAS","OATS","WHISCA")
plot(BVRT.g)

# For CERAD.Constructional.Praxis.Recall, edges are:
CCPR.g<-graph.full(4)
V(CCPR.g)$name<-c("AJCAR","OBAS","WHIMS","WHISCA")
plot(CCPR.g)

# For CERAD.Word.List.Memory, edges are:
CWLM.g<-graph.full(6)
V(CWLM.g)$name<-c("AJCAR","COURAGE","OBAS","SATSA","WHIMS","WHISCA")
plot(CWLM.g)

# For Delayed.Word.Recall, edges are:
DWR.g<-graph.full(12)
V(DWR.g)$name<-c("DCS_1905","10_66","ELSA","HAPIEE","HRS","JSTAR","LBLS","MIDUS",   
                 "CogUSA","PATH","SATSA","TILDA")
plot(DWR.g)

# For Forward.Digit.Span.Task, edges are:
FDST.g<-graph.full(6)
V(FDST.g)$name<-c("AJCAR","DCS_1905","OATS","OBAS","SATSA","WHISCA")
plot(FDST.g)

# For Hopkins.Verbal.Learning.Test, edges are:
HVLT.g<-graph.full(2)
V(HVLT.g)$name<-c("ACTIVE","SCS")
plot(HVLT.g)

# For Immediate.Recall.Word.List, edges are:
IRWL.g<-graph.full(13)
V(IRWL.g)$name<-c("DCS_1905","10_66","ELSA","HAPIEE","HRS","JSTAR","LBLS","MIDUS",   
                  "CogUSA","PATH","SNAC_K","TILDA","WLS")
plot(IRWL.g)

# For Prospective.Long.Term.Memory.Test, edges are:
PLTMT.g<-graph.full(2)
V(PLTMT.g)$name<-c("ELSA","UKBiobank")
plot(PLTMT.g)

# For Prose.Recall, edges are:
PR.g<-graph.full(2)
V(PR.g)$name<-c("LBLS","OCTO_Twin")
plot(PR.g)

# For Rivermead.Behavioral.Memory.Test, edges are:
RBMT.g<-graph.full(2)
V(RBMT.g)$name<-c("ACTIVE","CaPS")
plot(RBMT.g)

# For Rey.Auditory.Verbal.Learning.Test, edges are:
RAVLT.g<-graph.full(7)
V(RAVLT.g)$name<-c("ACTIVE","CLSA","CSHA","LASA","SydneyMAS","OATS","SEBAS")
plot(RAVLT.g)

# For Thurstone's.Picture.Memory.Test, edges are:
TPMT.g<-graph.full(2)
V(TPMT.g)$name<-c("OCTO_Twin","SATSA")
plot(TPMT.g)

# For Verbal.Learning.Test, edges are:
VLT.g<-graph.full(4)
V(VLT.g)$name<-c("HELIAD","MCSA","PROSPER","WHISCA")
plot(VLT.g)

# For WAIS.Digit.Span, edges are:
WDS.g<-graph.full(14)
V(WDS.g)$name<-c("COURAGE","CSHA","LASA","LBC1936","LGP","MCSA","OBAS",     
                 "OCTO_Twin","SAGE","SATSA","SEBAS","SNAC_K","UKBiobank","WLS")
plot(WDS.g)

# For Wechsler.Memory.Scale.Face.recognition, edges are:
WMSFR.g<-graph.full(2)
V(WMSFR.g)$name<-c("OBAS","SATSA")
plot(WMSFR.g)

# For Wechsler.Memory.Scale.Letter.number, edges are:
WMSLN.g<-graph.full(3)
V(WMSLN.g)$name<-c("LBC1936","OBAS","VETSA")
plot(WMSLN.g)

# For Wechsler.Memory.Scale.Logical.memory, edges are:
WMSLM.g<-graph.full(9)
V(WMSLM.g)$name<-c("CFAS","CSHA","LBC1936","LGP","SydneyMAS","MCSA","OATS",     
                   "OBAS","VETSA")
plot(WMSLM.g)

# For Wechsler.Memory.Scale.Spatial.Span, edges are:
WMSSS.g<-graph.full(2)
V(WMSSS.g)$name<-c("LBC1936","VETSA")
plot(WMSSS.g)

# For Wechsler.Memory.Scale.Visual.reproduction, edges are:
WMSVR.g<-graph.full(3)
V(WMSVR.g)$name<-c("MCSA","OBAS","VETSA")
plot(WMSVR.g)

# For Word.Recognition, edges are:
WR.g<-graph.full(4)
V(WR.g)$name<-c("LBLS","SATSA","SNAC_K","WHIMS")
plot(WR.g)

# For Working.Memory.Span, edges are:
WMS.g<-graph.full(2)
V(WMS.g)$name<-c("CSHA","LBLS")
plot(WMS.g)

# create a single graph with edges having different weights
titi<-rbind( get.edgelist(BVRT.g),
       get.edgelist(CCPR.g),
       get.edgelist(CWLM.g),
       get.edgelist(DWR.g),
       get.edgelist(FDST.g),
       get.edgelist(HVLT.g),
       get.edgelist(IRWL.g),
       get.edgelist(PLTMT.g),
       get.edgelist(PR.g),
       get.edgelist(RBMT.g),
       get.edgelist(RAVLT.g),
       get.edgelist(TPMT.g),
       get.edgelist(VLT.g),
       get.edgelist(WDS.g),
       get.edgelist(WMSFR.g),
       get.edgelist(WMSLN.g),
       get.edgelist(WMSLM.g),
       get.edgelist(WMSSS.g),
       get.edgelist(WMSVR.g),
       get.edgelist(WR.g),
       get.edgelist(WMS.g)
 )
titi<-graph(t(titi),directed=F)
E(titi)$weight<-1      # set weight=1
titi<-simplify(titi)   # convert multi-graph to weighted simple graph
E(titi)$weight
E(titi)$color<-E(titi)$weight
E(titi)$width<-E(titi)$weight
V(titi)$color<-"yellow"
plot(titi,edge.width=E(titi)$width,edge.color=E(titi)$color,
                     layout=layout.kamada.kawai)

# create a bipartite graph with constructs as members of studies
# I changed 10_66 for Ten_66
# I excluded ALSA and KOCOA
titi.bip<-graph.formula( BVRT:CCPR:CWLM:DWR:FDST:HVLT:IRWL:PLTMT:PR:RBMT:
                         RAVLT:TPMT:VLT:WDS:WMSFR:WMSLN:WMSLM:WMSSS:WMSVR:WR:WMS,
                ACTIVE:AJCAR:CaPS:CFAS:CLSA:COURAGE:CSHA:DCS_1905:Ten_66:ELSA:HAPIEE:HELIAD:HRS:      
                JSTAR:LASA:LBC1936:LBLS:LGP:SydneyMAS:MCSA:MIDUS:CogUSA:OATS:OBAS:OCTO_Twin:PATH:     
                PROSPER:SAGE:SATSA:SEBAS:SNAC_K:SCS:TILDA:UKBiobank:VETSA:WHIMS:WHISCA:WLS, 
          CSHA:SydneyMAS:OATS:WHISCA - BVRT,
          AJCAR:OBAS:WHIMS:WHISCA - CCPR,          
          AJCAR:COURAGE:OBAS:SATSA:WHIMS:WHISCA - CWLM,
          DCS_1905:Ten_66:ELSA:HAPIEE:HRS:JSTAR:LBLS:MIDUS:CogUSA:PATH:SATSA:TILDA - DWR,
          AJCAR:DCS_1905:OATS:OBAS:SATSA:WHISCA - FDST,
          ACTIVE:SCS - HVLT,
          DCS_1905:Ten_66:ELSA:HAPIEE:HRS:JSTAR:LBLS:MIDUS:CogUSA:PATH:SNAC_K:TILDA:WLS - IRWL,
          ELSA:UKBiobank - PLTMT,
          LBLS:OCTO_Twin - PR,
          ACTIVE:CaPS - RBMT,
          ACTIVE:CLSA:CSHA:LASA:SydneyMAS:OATS:SEBAS - RAVLT,
          OCTO_Twin:SATSA - TPMT,
          HELIAD:MCSA:PROSPER:WHISCA - VLT,
          COURAGE:CSHA:LASA:LBC1936:LGP:MCSA:OBAS:OCTO_Twin:SAGE:SATSA:SEBAS:SNAC_K:UKBiobank:WLS - WDS,
          OBAS:SATSA - WMSFR,
          LBC1936:OBAS:VETSA - WMSLN,
          CFAS:CSHA:LBC1936:LGP:SydneyMAS:MCSA:OATS:OBAS:VETSA - WMSLM,
          LBC1936:VETSA - WMSSS,
          MCSA:OBAS:VETSA - WMSVR,
          LBLS:SATSA:SNAC_K:WHIMS - WR,
          CSHA:LBLS - WMS )

V(titi.bip)$type<-as.logical(c(rep(1,21),rep(0,38)))
V(titi.bip)$size<-ifelse(V(titi.bip)$type,2*graph.strength(titi.bip),10)
V(titi.bip)$label.dist<-ifelse(V(titi.bip)$type,0,0)
V(titi.bip)$label.color<-ifelse(V(titi.bip)$type,"darkblue","red")
V(titi.bip)$label.cex<-ifelse(V(titi.bip)$type,1,0.8)
V(titi.bip)$label.font<-ifelse(V(titi.bip)$type,2,1)

plot(titi.bip, vertex.color=ifelse(V(titi.bip)$type,"cyan","yellow"),
               vertex.label=ifelse(V(titi.bip)$type,V(titi.bip)$name,1:38),
#               layout=layout.kamada.kawai
                layout=layout.reingold.tilford(titi.bip,circular=T)
#                 layout=layout.fruchterman.reingold
)

plot(titi.bip, vertex.shape=ifelse(V(titi.bip)$type,"circle","circle"),
               vertex.color=ifelse(V(titi.bip)$type,"cyan","yellow"),
               layout=layout.bipartite(titi.bip)[,2:1] )

#############################################################################################################

###############################################################
# MODIFIED analysis for memory scale
# read table of Memory scale-coverage_IALSA (I have generated from MR search)
# REMOVE all dashes in names of either constructs or .studies
##############################################################
 
memory.IALSA<-read.csv(file="C:\\Users\\ghislain\\Documents\\cognitive-scales-IALSA\\Memory-scale-coverage-IALSA.csv",
                 header=T,sep=",",na.strings="")
memory.IALSA<-memory.IALSA[,-c(2,3)]

which(apply(memory.IALSA[,-1]!=0,2,sum)==0)    # to identify constructs not used by any study
## Exclude 9, 18, 33

which(apply(memory.IALSA[,-1]!=0,2,sum)==1)    # to identify constructs used by only 1 study
## Exclude 1,2,4,7,8,14,15,16,17,19,22,24,25,26,28,30,31,34,39

memory2<-memory.IALSA[,-c(2,3,5,8,9,10,15,16,17,18,19,20,23,25,26,27,29,31,32,34,35,40)] 
which(apply(memory2[,-1]!=0,2,sum)==1)   # OK 20 constructs remain
which(apply(memory2[,-1]!=0,1,sum)==0)   # exclude study 2 (ALSA) and 11 (KOCOA) with no constructs                                                                  
memory2<-memory2[-c(2,11),]              # 31 studies
                                                                      
library(sand)

titi2.bip<-graph.formula( BVRT:CCPR:CWLM:DWR:FDST:HVLT:IRWL:PR:RBMT:
                         RAVLT:TPMT:VLT:WDS:WMSFR:WMSLN:WMSLM:WMSSS:WMSVR:WR:WMS,
                ACTIVE:CaPS:CFAS:CLSA:CSHA:DCS_1905:ELSA:HELIAD:HRS:      
                LASA:LBC1936:LBLS:LGP:SydneyMAS:MCSA:MIDUS:CogUSA:OATS:OBAS:OCTO_Twin:PATH:     
                PROSPER:SATSA:SEBAS:SNAC_K:SCS:TILDA:VETSA:WHIMS:WHISCA:WLS, 
          CSHA:SydneyMAS:OATS:WHISCA - BVRT,
          OBAS:WHIMS:WHISCA - CCPR,          
          OBAS:SATSA:WHIMS:WHISCA - CWLM,
          DCS_1905:ELSA:HRS:LBLS:MIDUS:CogUSA:PATH:SATSA:TILDA - DWR,
          DCS_1905:OATS:OBAS:SATSA:WHISCA - FDST,
          ACTIVE:SCS - HVLT,
          DCS_1905:ELSA:HRS:LBLS:MIDUS:CogUSA:PATH:SNAC_K:TILDA:WLS - IRWL,
          LBLS:OCTO_Twin - PR,
          ACTIVE:CaPS - RBMT,
          ACTIVE:CLSA:CSHA:LASA:SydneyMAS:OATS:SEBAS - RAVLT,
          OCTO_Twin:SATSA - TPMT,
          HELIAD:MCSA:PROSPER:WHISCA - VLT,
          CSHA:LASA:LBC1936:LGP:MCSA:OBAS:OCTO_Twin:SATSA:SEBAS:SNAC_K:WLS - WDS,
          OBAS:SATSA - WMSFR,
          LBC1936:OBAS:VETSA - WMSLN,
          CFAS:CSHA:LBC1936:LGP:SydneyMAS:MCSA:OATS:OBAS:VETSA - WMSLM,
          LBC1936:VETSA - WMSSS,
          MCSA:OBAS:VETSA - WMSVR,
          LBLS:SATSA:SNAC_K:WHIMS - WR,
          CSHA:LBLS - WMS )

V(titi2.bip)$type<-as.logical(c(rep(1,20),rep(0,31)))
V(titi2.bip)$size<-ifelse(V(titi2.bip)$type,2*graph.strength(titi2.bip),10)
V(titi2.bip)$label.dist<-ifelse(V(titi2.bip)$type,0,0)
V(titi2.bip)$label.color<-ifelse(V(titi2.bip)$type,"darkblue","red")
V(titi2.bip)$label.cex<-ifelse(V(titi2.bip)$type,0.9,0.8)
V(titi2.bip)$label.font<-ifelse(V(titi2.bip)$type,2,1)

k = V(titi2.bip)
nm_k = names(k)
nm_k[21:51] = 1:31
par(mar=c(3,2,2,1))  # to get smaller margins in plotting region
plot(titi2.bip, vertex.color=ifelse(V(titi2.bip)$type,"cyan","yellow"),
                vertex.label=nm_k,
                edge.width=2,
#               layout=layout.kamada.kawai
                layout=layout.reingold.tilford(titi2.bip,circular=T)
#                 layout=layout.fruchterman.reingold
)
legend("bottomleft",paste(1:31,":", names(V(titi2.bip))[21:51]),
        cex=0.6,ncol=4,text.col="red",bty="n")

###########################################################################################


# Can we make the union of CRT and PC?
uni<-graph.union(CRT,PC)
plot(uni)
# yes, even if they are disjoint


uni<-graph.union(CRT,PC)
add.edges(uni,E(SRT))
E(uni)$weight

toto<-read.table(file="C:\\Users\\ghislain\\Documents\\cognitive-scales-IALSA\\Emotional distress.txt",
                 header=T,sep="\t")

titi.bip<-graph.formula( BSI:BABS:BPHQ:DSC:CESD:CIDI:GADSA:GADSD:GDS:GHQ:GADS:HADS:KPDS:NI:PNAS:
                         PGCMS:PHQ:PSS:SFPSS:SPAQ:SSTAI:SSTAS,
                         ACT:ACTIVE:Add.Health:ALSA:CaG:CaPS:CC75C:CLS:CLSA:CSHA:DCS.19x5:DEAS:
                         ELSA:HAS:HCS:HELIAD:HRS:HSAD:JHS:KOCOA:LASA:LBLS:LSOG:SydneyMAS:MCSA:
                         MIDUS:CogUSA:NHS:NLSY79:NuAge:OATS:OBAS:OCTO.Twin:PATH:SATSA:SEBAS:
                         SNAC.K:SCS:TILDA:ULSAM:VA.NAS:VETSA:WH.II:WHIMS:WHISCA:WLS,       
         

      LSOG:VA.NAS - BSI, LSOG:WH.II - BABS, SydneyMAS:NHS:PATH - BPHQ,
      CC75C:DCS.19x5:TILDA - DSC,
      ACT:ACTIVE:Add.Health:ALSA:CLSA:CSHA:DEAS:ELSA:HRS:JHS:LASA:LSOG:MIDUS:CogUSA:NHS:NLSY79:OBAS
      OCTO.Twin:SATSA:SEBAS:TILDA:VETSA:WH.II:WLS - CESD,
      PATH:WLS - CIDI, CLS:SydneyMAS:OATS:PATH - GADSA,
      CLS:CSHA:SydneyMAS:PATH - GADSD,
      CSHA:HELIAD:KOCOA:LBLS:SydneyMAS:NHS:NuAge:OATS:OBAS:ULSAM:WHIMS:WHISCA - GDS,
      CaPS:CC75C:ELSA:HSAD:WH.II - GHQ,
                  GADS,
                  HADS,
                  KPDS,
                  NI,
                  PNAS, 
                  PGCMS,
                  PHQ,
                  PSS,
                  SFPSS,
                  SPAQ,
                  SSTAI,
                  SSTAS



                     LBLS:SNAC.K - PC, CLS:OBAS - SRT, 
               ACTIVE:ALSA:SydneyMAS:OBAS:OCTO.Twin:PATH:SATSA - SDMT,
                 CLS:DEAS:OBAS - WDST )

toto[toto[,3]!="x",1]


