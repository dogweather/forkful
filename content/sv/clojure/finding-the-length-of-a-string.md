---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att hitta längden på en sträng innebär att räkna antalet tecken inom den. Programmerare gör detta för att hantera textdata korrekt, som att begränsa input eller dela upp text.

## Hur gör man:

Clojure ger oss en inbyggd funktion `count` för att hitta längden på en sträng. Testa det här:

```Clojure 
(defn sträng-längd [sträng]
  (count sträng))

(sträng-längd "Hej Sverige!")
"-> 12"
```

## Djupdykning:

Hitta stränglängden är en grundläggande programmeringsfunktion som har varit närvarande sedan de tidiga dagarna av programmering. I Clojure implementeras `count` med hjälp av Java String's `length`-metod, vilket är en konstanttidsoperation.

Som alternativ kan vi också räkna tecken med hjälp av en slinga, men det är inte lika effektivt. Här är ett exempel:

```Clojure
(defn sträng-längd-egen [sträng]
  (loop [tecken sträng längd 0]
    (if (empty? tecken) 
      längd
      (recur (subs tecken 1) (inc längd)))))

(sträng-längd-egen "Hej Sverige!")
"-> 12"
```
Observera att detta är onödigt och bara till för utbildningssyfte.

## Se också:

Om du vill veta mer om stränger i Clojure, kolla in följande resurser:

1. [Clojure Docs](https://clojuredocs.org/clojure.core/count)
2. [Stack Overflow - Counting Characters in Clojure](https://stackoverflow.com/questions/9862228/counting-characters-in-clojure)
3. [Cheatsheet för Clojure](https://clojure.org/api/cheatsheet)

Kom ihåg att de bästa kodlösningarna är effektiva och klara. Smidig kod innebär snabbare och mer förståelig kod.