---
title:                "Arbeta med YAML"
html_title:           "Bash: Arbeta med YAML"
simple_title:         "Arbeta med YAML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML är en filformat för att lagra och läsa datastrukturer. Det är populärt bland programmerare eftersom det är enkelt att läsa och skriva, vilket gör det till ett bra val för konfigurationsfiler och andra typer av datahantering.

## Hur man:
För att arbeta med YAML i Bash kan du använda verktyget "yq". Här är ett exempel på hur du kan läsa in en YAML-fil och hämta ett värde från den:

```Bash
# Läs in filen
data=$(cat fil.yaml)
# Hämta värdet för "nyckel"
value=$(echo $data | yq r - nyckel)
# Skriv ut värdet
echo $value
```

Det går också att använda andra kommandon, som "grep", för att filtrera data från YAML-filen. Till exempel:

```Bash
# Läs in filen
data=$(cat fil.yaml)
# Hämta värdet för "nyckel" endast om den innehåller "värde"
value=$(echo $data | grep "nyckel.*värde" | yq r - nyckel)
# Skriv ut värdet
echo $value
```

## Djupdykning:
YAML står för "YAML Ain't Markup Language" och är ett format som skapats för att vara mänskligt läsbart och enkelt för datorer att tolka. Det används ofta för konfigurationsfiler i olika projekt och har blivit populärt inom programmeringsvärlden på senare tid.

Ett annat populärt format för datahantering är JSON, men det kan vara svårare att läsa för människor. YAML har därför blivit ett populärt alternativ för att göra datastrukturen mer lättläst.

Det finns också andra sätt att arbeta med YAML i Bash, som att använda "yamllint" för att validera filer eller "yq merge" för att slå samman flera YAML-filer.

## Se även:
- [yq dokumentation](https://mikefarah.gitbook.io/yq/)
- [yamllint dokumentation](https://github.com/adrienverge/yamllint)
- [yq merge dokumentation](https://mikefarah.gitbook.io/yq/commands/merge)