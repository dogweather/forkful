---
title:                "Ta bort tecken som matchar ett mönster"
aliases:
- /sv/clojure/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:41:53.096448-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ta bort tecken som matchar ett mönster"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
I Clojure och andra programmeringsspråk betyder att ta bort tecken som matchar ett mönster att du rensar din textsträng från oönskade sekvenser. Detta görs ofta för datarensning, för att hantera användarinmatning, eller för att förbereda text för vidare bearbetning.

## How to:
Clojure ger oss `clojure.string/replace` för att hantera teckentvätt. Kolla här:

```Clojure
(require '[clojure.string :as str])

;; Ta bort alla siffror från en sträng.
(println (str/replace "Del4eta 42 non-num8ers!" #"\d" ""))
;; Output: "Deleta non-numers!"

;; Ta bort specialtecken, behåll bara bokstäver och siffror.
(println (str/replace "Hej! Är allt bra? 100% klart." #"[^\w\s]" ""))
;; Output: "Hej Är allt bra 100 klart"
```

## Deep Dive:
Funktionen `clojure.string/replace` finns i Clojure sedan de tidiga versionerna. Det är en Swig-hänskjuten metod som är både kraftfull och mångsidig. Även om `replace` är enkel att använda, har den två viktiga alternativ: Du kan skicka in ett enkelt strängmönster eller använda en `java.util.regex.Pattern` för mer komplexa ersättningsregler.

Implementationsmässigt utför `replace` sin magi genom att loopa över strängen och tillämpa mönstret du har specifierat. Detta görs effektivt och utan onödig belastning på systemet.

Alternativ till `str/replace` inkluderar att använda `re-seq` för att hitta alla förekomster som inte matchar mönstret och sedan bygga en ny sträng från dessa, men detta är vanligtvis mer kod och mindre rakt på sak.

## See Also:
- Clojure’s official string documentation: https://clojure.github.io/clojure/clojure.string-api.html
- Java Pattern class: https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html
- Online regex tester: https://regexr.com/ där du kan experimentera med regex före implementering.
