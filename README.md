# Psykometriska analyser av data från Stockholms enkäten  

Denna text kommer att uppdateras med mera specifik information om källkoden som finns lagrad här.

## Historik

RISE har på uppdrag av Länsstyrelsen i Stockholm tagit fram en kunskapssammanställning om risk- och skyddsfaktorer för multipla problemutfall samt insatser för att arbeta förebyggande och främjande. Nu pågår ett arbete för att kvalitetssäkra mätning av relevanta faktorer, samt visualisera data för användning i samverkan med kommuner.

Kunskapssammanställningen **Risk- och skyddsfaktorer – vad vet vi och vad kan göras med kunskapen?** finns tillgänglig i DIVA-databasen. Bakgrundsmaterial, inklusive källkod för att skapa figurerna i rapporten, finns att ladda ner från Open Science Frameworks hemsida: [https://osf.io/935b6/](https://osf.io/935b6/). Allt material har publicerats under den öppna licensen [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/) som innebär att det är fritt att dela och använda/vidareutveckla materialet, även för kommersiella syften, så länge hänvisning ges till rapporten.

Nästa steg i arbetet handlar om mätning av risk- och skyddsfaktorer, där Stockholmsenkäten spelar en central roll. Stockholmsenkäten genomförs varannat år genom samarbete med skolor, där årskurs 9 och gymnasiets åk 2 tillfrågas om anonymt deltagande. Ett omfattande arbete har genomförts för att psykometriskt utvärdera vilka index som kan användas för att representera risk- och skyddsfaktorer utifrån Stockholmsenkätens frågor, baserat på data från 2006 till 2020. Psykometriska analyser utförs primärt med Rasch-metodik, en form av modern testteori.

Mera information finns på: [https://www.ri.se/sv/vad-vi-gor/projekt/data-i-dialog-risk-och-skyddsfaktorer-for-barn-och-unga](https://www.ri.se/sv/vad-vi-gor/projekt/data-i-dialog-risk-och-skyddsfaktorer-for-barn-och-unga)

## Om data

Eftersom RISE inte har erhållit möjligheter att dela data som använts för dessa analyser och de rapporter som genereras i [kommunrapporterna inom Data i Dialog](https://github.com/pgmj/DIDreport) går det tyvärr inte att direkt reproducera analyserna. Det är respektive kommun som äger sina data, så den intresserade får höra av sig direkt till dem. Stockholm Stad har tillhandahållit datasetet som använts i de psykometriska analyserna.

Utifrån de psykometriska analyserna estimeras mätvärden inom de områden/index där tillräcklig mätkvalitet kunnat påvisas. Dessa mätvärden används sedan i genereringen av ovan nämnda kommunrapporter. Hur detta sker framgår nedan.

## Bearbetning av data utifrån psykometrisk analyser

Varje kapitel i denna rapport har analyser där källkoden beskriver omkodning av svarskategorier. De omkodningar som görs utifrån analyserna finns sedan samlade i mappen `OtherScripts`. Där finns även script för att importera data från flera kommuner och se till att de använder samma variabelnamn, etc. Dessa script har siffror i början på filnamnet (01 till 04). Scriptfil 03 estimerar mätvärden för alla index för alla individer, vilket kan ta lång tid när ett större dataset används.

Varje fil bygger på att filen/filerna innan har körts, eller att man manuellt importerat datafil(er) med den information som behövs för att köra filen.

### 01 SthlmsEnk gathering outputs.R

Denna fil läser in resultatet av alla psykometriska analyser och skriver dem till en datummärkt Microsoft Excel-fil, med kodraden:
``` r
write.xlsx(allItemInfo, glue("../DIDapp/data/{Sys.Date()}_allItemInfo.xls"), row.names = F)
```
Så länge inga ändringar görs i de psykometriska analyserna behöver inte denna fil uppdateras.

### 01b Import data.R 

Här importeras datafiler från alla kommuner och samordnas i struktur så de kan läggas samman i en och samma datafil. Funktionen `janitor::compared_df_cols()` borde använts i detta arbete, men kan i all fall rekommenderas för den som ska lägga in mera data senare.

### 02 SthlmsenkOmkodningar.R

Här kodas svarskategorierna om till siffror inför de psykometriska analyserna.

### 02b Recoded response categories.R 

Här implementeras de omkodningar av svarskategorier som de psykometriska analyserna har påvisat behöver göras.

### 03 Estimation of person locations.R   

I denna fil estimeras mätvärden för de index/områden som uppvisat adekvat mätkvalitet. Detta görs med en loop som går igenom alla index, vilket kan ta lång tid beroende på hur många respondenter som ingår i datafilen. Exempelvis tar 100 000 elever ca 1-2h att estimera.

I slutet på filen skrivs en datummärkt datafil:
``` r
write_parquet(df, sink = glue("../DIDapp/data/{Sys.Date()}_ScoredRev.parquet"))
```

Filformatet ".parquet" kan läsas och skrivas med paketet [`library(arrow)`](https://arrow.apache.org/docs/r/) som möjliggör både hög kompression och mycket snabb läs-/skrivhastighet.

### 04 Distributions and risk limits.R

Här beräknas gränsvärden för riskfaktorerna utifrån distributionen av data, och skrivs till CSV-filer med funktionen `write_csv()`. Detta används sedan som underlag för indelningen i riskgrupper i kommunrapporterna.

## Sambandsanalyser

Detta är "work in progress" som pågår i mappen `Samband`, där filen `SthlmsEnkSamband2.qmd` är en ansats att "renskriva" det mera explorativa analysarbetet som genomförts i `SthlmsEnkSamband.qmd`. En rapportfil kommer att skapas utifrån denna, och tillgängliggöras via GitHub Pages.