---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att parsa ett datum från en sträng innebär att extrahera och tolka datuminformation från en textsträng. Programmerare gör detta eftersom det är ett effektivt sätt att läsa in, bearbeta och skapa datuminformation från olika källor.

## Så här gör du:

Nedan är ett Bash exempel på hur man konverterar ett datumuttryck till en viss format.

```Bash
datum_sträng="2021-12-31"
datum_format="%Y-%m-%d"
strtotime=$(date -d "${datum_sträng}" +"${datum_format}")
echo $strtotime
```

Körning av denna kod ger följande output:

```Bash
2021-12-31
```

## Djupdykning:

1. Historisk kontext: Även om Bash ursprungligen inte designades för att parsa datum från strängar, har det genom tiden lagts till flera funktioner som kan göra detta. `date -d` är ett exempel på detta.

2. Alternativ: Det finns många sätt att parsa ett datum från en sträng i Bash. Förutom `date -d`, kan du också använda `date +%Y-%m-%d -d` eller specialiserade parsingverktyg som `dateutils`.

3. Implementeringsdetaljer: Att parsa ett datum från en sträng i Bash innebär att tolka strängen och konvertera den till en struktur som datorn kan förstå och arbeta med. Detta kan vara komplicerat när och om datumen finns i olika format.

## Se också:

För mer detaljerad information om ämnet, bekanta dig med följande resurser:

1. [GNU-coreutills: Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)
2. [Bash Guide för nybörjare](https://tldp.org/LDP/Bash-Beginners-Guide/html/chap_03.html)
3. [Bash manualsidan](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)