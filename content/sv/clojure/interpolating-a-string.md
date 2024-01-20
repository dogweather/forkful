---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Interpolering av strängar handlar om att infoga variabelvärden direkt i en sträng. Programmerare gör det för att sätta samman dynamiska strängar på ett snyggt och lättläst sätt.

## Så här gör du:

Clojure erbjuder inte inbyggd stränginterpolering. Istället kan du använda `format`, vilket fungerar som `printf` i andra språk. Se kodexempel och utdata nedan:

```Clojure
(def name "Stina")
(def age 30)

(format "Hej, jag heter %s och jag är %d år gammal." name age)
```

Kodens output blir: `Hej, jag heter Stina och jag är 30 år gammal.`

## Djupdykning

Den här metoden för interpolering härstammar från språk som C, vilka har stöd för printf-stil av formatering. Det finns andra sätt att hantera stränginterpolation i Clojure, som att använda `str` eller concatenation med `join`, men `format` erbjuder en renare syntax för komplexa strängar. Med försiktighet kan du också använda libraries som `clojure.string/replace` för att realisera interpolering, men var försiktig med säkerhetsrisker som kan uppstå med en sådan approach.

## Se även

Relaterade resurser finns länkade här nedan:

- Clojure Documentation: [https://clojure.org/guides/learn/functions#formatting](https://clojure.org/guides/learn/functions#formatting)
- Clojure String library: [https://clojuredocs.org/clojure.string](https://clojuredocs.org/clojure.string)
- "Clojure for the Brave and True": [https://www.braveclojure.com/core-functions-in-depth/](https://www.braveclojure.com/core-functions-in-depth/)