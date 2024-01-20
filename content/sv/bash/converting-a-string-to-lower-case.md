---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Bash Programming: Omvandla Strängar till Små Bokstäver

## Vad och Varför?
Att omvandla en sträng till små bokstäver innebär att ändra varje stor bokstav i en textsträng till dess motsvarande lilla bokstav. Som programmerare gör vi detta för att standardisera inmatning och undvika problem med skiftlägeskänslighet.

## Så här gör du:
I Bash kan du använda `tr` kommandot för att omvandla en sträng till små bokstäver. Här är ett exempel:

```Bash
STR="Hej, Vänner!"
echo $STR | tr '[:upper:]' '[:lower:]'
```

Detta ger följande utskrift:

```Bash
hej, vänner!
```

## Djupdykning
Bash har stöd för omvandling av strängar till små bokstäver sedan version 4.0, släppt 2009. Alternativt kan `awk` och `sed` användas för samma ändamål. Men `tr` anses vara det snabbaste och mest effektiva sättet. Dessutom är `tr` implementerat på ett sådant sätt att det kan hantera stora datamängder utan signifikanta prestandaproblem.

## Se även
1. Bash manual, tr command: https://www.gnu.org/software/bash/manual/html_node/The-Shopt-Builtin.html
2. GNU Text Utilities, tr: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
3. Convert A String To Lowercase - https://www.cyberciti.biz/faq/linux-unix-convert-string-to-lowercase