---
title:                "Arbeta med CSV"
aliases:
- /sv/bash/working-with-csv.md
date:                  2024-02-03T19:18:48.249504-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeta med CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med CSV-filer (kommaseparerade värden) i Bash handlar om att bearbeta och manipulera tabellformaterade data lagrade i ren textformat. Detta är avgörande för programmerare eftersom det möjliggör automatisering av uppgifter relaterade till datatransformering, analys och integration direkt från kommandoraden, utan behovet av tyngre verktyg eller programmeringsmiljöer.

## Hur man gör:

**Läsa en CSV-fil rad för rad**

```bash
while IFS=, read -r kolumn1 kolumn2 kolumn3
do
  echo "Kolumn 1: $kolumn1, Kolumn 2: $kolumn2, Kolumn 3: $kolumn3"
done < sample.csv
```

*Exempel på utdata:*

```
Kolumn 1: id, Kolumn 2: namn, Kolumn 3: email
...
```

**Filtrera CSV-rader baserat på ett villkor**

Med `awk` kan du enkelt filtrera rader. Till exempel för att hitta rader där den andra kolumnen är lika med "Alice":

```bash
awk -F, '$2 == "Alice" { print $0 }' sample.csv
```

**Modifiera ett kolumnvärde**

För att ändra andra kolumnen till versaler:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' sample.csv
```

**Sortera en CSV-fil baserat på en kolumn**

Du kan sortera en CSV-fil baserat på, säg, den tredje kolumnen (numeriskt):

```bash
sort -t, -k3,3n sample.csv
```

**Använda `csvkit` för mer komplexa uppgifter**

`csvkit` är en svit av kommandoradsverktyg för att konvertera till och arbeta med CSV. Det kan installeras via pip.

För att konvertera en JSON-fil till CSV:

```bash
in2csv data.json > data.csv
```

För att fråga en CSV-fil med SQL:

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" sample.csv
```

*Obs: Installation av `csvkit` kräver Python och kan göras med hjälp av `pip install csvkit`.*
