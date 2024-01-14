---
title:                "Bash: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Det finns många anledningar till varför någon skulle vilja söka och ersätta text med hjälp av Bash-programmering. Kanske behöver du ändra namnen på filer eller uppdatera dokument med ny information? Oavsett anledningen kan sök- och ersättningsfunktionen i Bash vara ett mycket praktiskt verktyg för att effektivisera och automatisera uppgifter.

## Hur man gör

För att utföra en sök- och ersättningsuppgift i Bash behöver du använda kommandot "sed", vilket står för "stream editor". Nedan följer ett enkelt exempel på hur du kan söka och ersätta ett ord i en fil:

```Bash
sed 's/gammalt_ord/nytt_ord/g' fil.txt 
```

Detta kommando kommer att söka efter alla instanser av "gammalt_ord" i filen "fil.txt" och ersätta dem med "nytt_ord". Det sista "g" står för "global" och innebär att alla instanser av det gamla ordet kommer att ersättas, inte bara den första.

Om du vill att sök- och ersättningsuppgiften endast ska utföras på vissa rader i filen kan du använda flaggan "-e" för att specificera ett villkor. Till exempel:

```Bash
sed -e'/villkor/ s/gammalt_ord/nytt_ord/g' fil.txt
```

I detta fall kommer endast rader som uppfyller villkoret att sökas igenom och ersättningen utförs endast på dessa rader.

## Djupdykning

Det finns många olika användningsområden för sök- och ersättningsfunktionen i Bash. Det finns också flera olika flaggor och parametrar som du kan använda för att anpassa din sök- och ersättningsuppgift ytterligare. Till exempel kan du använda flaggan "-i" för att ignorera skillnader i bokstavsstorlek eller "-n" för att visa vilka rader som har matchats och ersatts.

Du kan också använda regelbundna uttryck (regular expressions) för att söka och ersätta text i Bash. Detta öppnar upp för ännu fler möjligheter och komplexa sökningar. Genom att lära dig mer om regelbundna uttryck kan du göra dina sök- och ersättningsuppgifter ännu kraftfullare.

## Se även

För mer information om Bash-programmering och användning av "sed" för sök- och ersättningsuppgifter, se följande länkar:

- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Sed - An Introduction and Tutorial](https://www.gnu.org/software/sed/manual/sed.html)
- [Regular Expressions - The Basics](https://www.regular-expressions.info/tutorial.html)