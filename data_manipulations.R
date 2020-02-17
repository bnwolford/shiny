df<-fread('all_phecodes_ukbb_phenodefs.txt.gz')
    names(df)<-names(df)%>%make.unique()
    names(df)[1]<-'Chr'
    ##Change this: select columns you care about for analysis
    df<-df%>%select(Chr,POS,ID,REF,ALT,af,num_cases,num_controls,pval,phecode,group,description)%>%
        mutate(p=-log(pval,base=10),
               rowid=seq(n()))
fwrite(df, file='ukbb_phecodes_cleaned.csv')
