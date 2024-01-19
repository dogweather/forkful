---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hantera Strängar i Clojure: Så drar du ut delsträngar

## Vad & Varför?

Extrahering av delsträngar innebär att ta ut och förmedla specifika delar av en sträng. Programmerare gör detta för att manipulera och granska data mer effektivt.

## Hur Man Gör:
För att utvinna delsträngar i Clojure, använd funktionen `subs`, som tar två argument: strängen och det startindex du vill börja på. Låt oss titta på några kodexempel.

```Clojure
(def message "Hej, Värld!")
(subs message 0 3)
```
Resultat:

```Clojure
"Hej"
```
Här tar vi ut delsträngen "Hej" från strängen "Hej, Värld!".

Nu, låt oss ta en delsträng från mitten av en sträng:

```Clojure
(subs message 5)
```
Resultat:

```Clojure
"Värld!"
```

## Djupdykning:

Historiskt sett har extrahering av delsträngar varit ett grundläggande inslag i många programmeringsspråk, eftersom det möjliggör analys och bearbetning av textdata.

I Clojure finns det även en annan metoden `get`, den kan anpassa för att extrahera specifika tecken baserat på deras index, men det returnerar en karaktär, inte en sträng.

Implementationen av `subs` i Clojure bygger på Java's substring-metod, vilket innebär att den är både snabb och effektiv.

## Se Även: 

För en djupdykning i strängmanipulering och bearbetning i Clojure, rekommenderas följande länkar:

1. [Clojure Docs om Strängbearbetning](https://clojure.org/guides/data_string)
2. [Clojure for the Brave and True: Kapitel om Strängar](https://www.braveclojure.com/core-functions-in-depth)