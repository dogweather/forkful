---
title:                "Använda reguljära uttryck"
html_title:           "Gleam: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Enkelt uttryckt, användningen av reguljära uttryck (regex) är processen att matcha textsträngar baserat på specifika mönster. Programmerare utför detta för att sålla, identifiera eller ersätta specifika datastycken i en text.

## Hur gör man:
Här är ett grundläggande exempel på hur du kan använda regex i Clojure:

```clojure
(defn matcher [pattern str]
  (re-seq (re-pattern pattern) str))
```
Om vi kör metoden `matcher` med ordet "apples" och strängen "apples are yummy and apples are red", vi skulle få:

```clojure
(print (matcher "apples" "apples are yummy and apples are red"))
```

Resultatet skulle vara `("apples" "apples")`.

## Djupdykning:
1. Historiskt sammanhang: Regex började användas i UNIX-världen på 1960-70-talet. Sedan dess används det i nästan alla programmeringsspråk, inklusive Clojure.
2. Alternativ: Det finns bibliotek som Specter som kan användas för komplex databehandling. Men när det gäller textbearbetning är regex fortfarande svårslagen.
3. Implementeringsdetaljer: Clojure använder Javas regex-motor som tillhandahåller två grundläggande regex-funktioner `re-matcher` och `re-pattern`.

## Se också:
För mer detaljerade information om regex i Clojure, kolla in dessa länkar:
- ClojureDoc - Regular Expressions: https://clojuredocs.org/clojure.core/re-matcher
- The Clojure Cookbook: https://www.clojure-cookbook.com/recipes/regular-expressions
- Clojure från början - Regular Expressions: https://kimh.github.io/clojure-by-example/#regular-expressions