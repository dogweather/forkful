---
title:                "Arbeta med csv"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Arbeta med CSV (Comma-Separated Values) innebär att hantera data i textformat där varje rad representerar en post och varje post är uppdelad med kommatecken. Programmerare använder CSV för att enkelt överföra och manipulera data mellan olika system.

## How to:

### Läsa en CSV-fil
```Fish Shell
for row in (cat example.csv)
    echo $row
end
```
### Exempelutdata
```
namn,ålder,stad
Alice,30,Stockholm
Bob,25,Göteborg
```

### Skriva till en CSV-fil
```Fish Shell
set -l users 'namn,ålder,stad' 'Alice,30,Stockholm' 'Bob,25,Göteborg'
for user in $users
    echo $user >> users.csv
end
```

### Filtrera data
```Fish Shell
cat example.csv | string match -r '.*Göteborg.*'
```
### Exempelutdata
```
Bob,25,Göteborg
```

## Deep Dive

Historiskt sett har CSV-formatet varit ett enkelt och utbrett sätt att lagra strukturerad data sedan de första versionerna av persondatorer. Alternativ till CSV inkluderar JSON, XML och YAML, men CSV förblir populärt för dess enkelhet och breda stöd i mjukvaror. I Fish Shell hanterar man CSV-data primärt genom textbearbetningskommandon som `cat`, `echo`, och `string`, istället för dedikerade CSV-parserverktyg.

## See Also

- Fish Shell dokumentation: https://fishshell.com/docs/current/index.html
- CSV-specifikationen: https://tools.ietf.org/html/rfc4180
- Support for working with tabular data in Fish: https://github.com/jorgebucaran/fish-tabular
