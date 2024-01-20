---
title:                "Söka och ersätta text"
html_title:           "Bash: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Sök och Ersätt Text i Bash

## Vad & Varför?

Sökning och ersättning av text är för att manipulera data. Programmerare använder det för att ändra koden snabbt, bearbeta filer och automatisera uppgifter.

## Hur?

Här är exempel på att söka och ersätta text i Bash:

```Bash
text='Hej, världen!'
echo ${text/Hej/Hejdå}
```

Ovanstående kod söker efter "Hej" i textsträngen och byter ut det mot "Hejdå". Resultatet blir 'Hejdå, världen!'.

Du kan också byta ut alla förekomster med följande:

```Bash
echo ${text//a/o}
```

Detta byter ut alla "a" till "o" i textsträngen.

## Djupdykning

Sök och ersätt-funktionen i Bash härrör från sed, ett tidigt Unix-verktyg. Alternativ inkluderar Perl och Python skript. Även om Bash inte är lika kraftfull som dessa, är det tillräckligt för enkla sök- och ersättningsuppgifter.

Principen för Bash sök och ersätt är att använda parameterexpansion. I `${parameter/pattern/string}`, `parameter` är variabeln att bearbeta, `pattern` är strängen att hitta, och `string` är ersättningen.

## Se Även

För mer information, kolla de här länkarna:

1. GNU Bash Manual: [Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
2. Sed & Awk, 2nd Edition: [Sed](https://www.oreilly.com/library/view/sed-awk-2nd/1565922255/)
3. Bash Guide for Beginners: [Manipulating Strings](https://tldp.org/LDP/Bash-Beginners-Guide/html/chap_10.html)