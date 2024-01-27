---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck, ofta förkortat regex, är mönster som hjälper till att hitta, matcha och hantera textsträngar. Programmerare använder det för att effektivisera textbearbetning, validera input eller göra komplexa textmanipulationer.

## How to:
Clojure använder Java's `java.util.regex.Pattern` för reguljära uttryck. Nedan är grunderna för att söka och ersätta text.

**Sökning:**
```clojure
(re-find #"\b[Cc]lojure\b" "Jag älskar att programmera i Clojure.")
;; => "Clojure"
```

**Hitta alla matchningar:**
```clojure
(re-seq #"[Ss]v(ensk[a]?)\b" "Svenska är roligt, och svenska filmer är intressanta.")
;; => ("Svenska" "svenska")
```

**Ersätt text:**
```clojure
(clojure.string/replace "Ersätt Java med Clojure!" #"\bJava\b" "Clojure")
;; => "Ersätt Clojure med Clojure!"
```

## Deep Dive
Reguljära uttryck har sitt ursprung i teoretisk datalogi och formell språkteori. Alternativ till regex inkluderar strängmetoder som `contains?`, `starts-with?`, och `ends-with?`. Clojure's regex-funktionalitet är en direkt bro till Java's `Pattern`-klass, vilket betyder att alla Java regex-funktioner kan användas direkt i Clojure.

## See Also
- [Clojure's official documentation on regex](https://clojure.org/guides/learn/functions#_regex)
- [Java Pattern class documentation](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/tutorial.html), en omfattande guide till regex.
