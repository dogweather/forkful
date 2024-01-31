---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är mönster för att matcha textsträngar i data. Programmerare använder det för att söka, extrahera och manipulera specifik text snabbt och effektivt.

## Hur gör man:
Exempel 1: Sök efter ordet "fisk" i en fil.
```Bash
grep "fisk" min-fil.txt
```
Exempel 2: Byt ut "katt" mot "hund" i en sträng.
```Bash
echo "Jag gillar katter mer än hundar" | sed 's/katt/hund/'
```
Exempel 3: Finn alla filer med .txt-suffix.
```Bash
ls *.txt
```

## Djupdykning
Reguljära uttryck har används sedan 1950-talet, med rötter i teoretisk datalogi och formaliserade i automatteori. Alternativ till Bash inkluderar Perl och Python som erbjuder ännu mer kraftfulla och flexibla regexp-implementeringar. Bash använder GREP och SED för regexp, vilka är kraftfulla men kan skilja sig i syntax mellan versioner och system.

## Se även
- GNU GREP manual: https://www.gnu.org/software/grep/manual/grep.html
- SED - Stream Editor: https://www.gnu.org/software/sed/manual/sed.html
- RegExr - Lär dig och testa reguljära uttryck: https://regexr.com/
