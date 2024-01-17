---
title:                "Att arbeta med csv"
html_title:           "PowerShell: Att arbeta med csv"
simple_title:         "Att arbeta med csv"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV (Comma Separated Values) är ett vanligt filformat för att lagra och utbyta data i tabellform. Det är vanligtvis används för att exportera och importera data mellan olika program eller system. Programmerare använder CSV för att enkelt hantera en stor mängd data och utföra åtgärder som filtrering och manipulation.

## Så här gör du:

Att arbeta med CSV i PowerShell är både enkelt och effektivt. Här är några vanliga åtgärder och exempel på kod:

1. Importera en CSV-fil: 
```PowerShell
Import-Csv -Path "C:Users\User\Desktop\data.csv" 
```
Resultatet kommer att returnera en objektlista baserad på data som finns i filen.

2. Skapa en CSV-fil: 
```PowerShell
$books = Get-ChildItem | Select-Object Name, CreationTime, Length 
$books | Export-Csv -Path "C:Users\User\Desktop\book_info.csv" 
```
Detta exempel skapar en CSV-fil med information om böcker i en viss mapp.

3. Lägga till data i en befintlig CSV-fil: 
```PowerShell
$data = "Name, Age, City" 
$data | Out-File -FilePath "C:Users\User\Desktop\people.csv" -Append 
```
Här lägger vi till data för en ny person i slutet av en befintlig CSV-fil.

För mer detaljerad information och fler exempel, besök Microsofts dokumentationssida om CSV-hantering i PowerShell.

## Djupdykning:
CSV-formatet uppfanns på 1970-talet och var det första formatet som användes för att lagra data i textform. Idag används det fortfarande flitigt för att utbyta information mellan program och system.

Det finns också andra sätt att hantera tabellformad data i PowerShell, som till exempel att använda objekt av typen [System.Data.DataTable]. Detta ger programmerare mer flexibilitet och möjlighet att hantera mer komplexa datastrukturer.

När du arbetar med CSV är det viktigt att hålla koll på separatorn som används i filen (vanligtvis ett kommatecken) och att se till att alla kolumner är korrekt formaterade. Det finns också möjlighet att exportera data som CSV med andra typer av separatorer, som till exempel semikolon eller tabb.

## Se även:
- Microsofts dokumentation om CSV-hantering i PowerShell: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv
- Skillnaden mellan CSV och andra dataformat i PowerShell: https://4sysops.com/archives/use-powershell-to-work-with-csv-formatted-data
- Användningsfall för CSV i PowerShell: https://devblogs.microsoft.com/scripting/build-secret-csv-parser