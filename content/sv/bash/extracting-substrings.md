---
title:                "Extrahering av delsträngar"
html_title:           "Bash: Extrahering av delsträngar"
simple_title:         "Extrahering av delsträngar"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Vi använder ofta Bash för att hantera och manipulera textsträngar, men ibland vill vi bara extrahera en del av en textsträng istället för hela strängen. Genom att lära dig hur man extraherar substringar i Bash kan du effektivt hantera och manipulera text på ett mer precist sätt.

## Så här gör du

Det finns två vanliga metoder för att extrahera substringar i Bash: använda substringsyntaxen eller använda inbyggda verktyg som cut eller sed.

För att använda substringsyntaxen kan du använda det inbyggda kommandot "expr". Till exempel, om vi vill extrahera de första tre tecknen i en sträng "Hello", kan vi skriva:

```Bash
expr substr "Hello" 1 3
```

Vilket ger oss utmatningen "Hel". Notera att det första numret (1) motsvarar startpositionen, medan det andra numret (3) motsvarar längden på den önskade substringen.

En annan vanlig metod är att använda verktyget "cut". Om vi till exempel vill extrahera de tre sista tecknen i samma sträng, kan vi skriva:

```Bash
echo "Hello" | cut -c 3-
```

Denna kommando tar input från "echo" och använder flaggan "-c" för att extrahera tecken från den tredje positionen och framåt, vilket ger oss utmatningen "llo".

Det finns också andra användbara verktyg som sed, awk och grep som kan användas för att extrahera substringar baserat på olika mönster.

## Fördjupa dig

När du behärskar dessa grundläggande metoder för att extrahera substringar i Bash kan du också utforska mer avancerade funktioner, som till exempel att extrahera från slutet av en sträng eller använda reguljära uttryck för att hitta mönster för extraherade substringar.

En annan viktig aspekt att tänka på är att Bash använder nollindexering för positioner i en sträng, vilket betyder att det första tecknet är på position 0, det andra är på position 1 och så vidare. Detta är viktigt att komma ihåg när du använder substringsyntaxen eller något av de inbyggda verktygen som cut.

## Se även

- [Bash Guide for Beginners](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_10_05.html)
- [Linuxize: Bash Substring Extraction](https://linuxize.com/post/bash-substring-extraction/)
- [The Linux Documentation Project: Command line processing](https://www.tldp.org/LDP/abs/html/cmdsub.html)