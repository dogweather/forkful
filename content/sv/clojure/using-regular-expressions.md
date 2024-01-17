---
title:                "Att använda reguljära uttryck"
html_title:           "Clojure: Att använda reguljära uttryck"
simple_title:         "Att använda reguljära uttryck"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Reguljära uttryck, även kallade regex, är ett kraftfullt verktyg inom programmering som låter dig söka efter mönster i text. Genom att använda regex kan du enkelt hitta och manipulera specifika delar av en sträng istället för att behöva gå igenom allt manuellt. Detta sparar tid och gör din kod mer effektiv.

## Hur man gör det:

```Clojure
;; Skapa ett reguljärt uttryck som matchar ordet "hej"
(def regex #"hej")

;; Hämta alla matchningar av uttrycket från en sträng
(re-seq regex "Hej på dig! Hej igen!")

;; Output: ("hej" "hej") 

```

## Djupdykning:

Reguljära uttryck har funnits sedan 1950-talet och används i många olika programmeringsspråk, inte bara Clojure. Alternativ till regex inkluderar olika inbyggda funktioner för strängbehandling, men regex ger ett mer kraftfullt och flexibelt sätt att hantera text. I Clojure implementeras regex genom användning av Java-biblioteket java.util.regex.

## Se även:

- [The Clojure Regex cheatsheet](https://clojure.org/api/cheatsheet/#Regex_Cheat_Sheet) för en snabb referens till regex funktioner i Clojure.
- [Regular-Expressions.info](https://www.regular-expressions.info/) för en grundlig och interaktiv guide till reguljära uttryck.