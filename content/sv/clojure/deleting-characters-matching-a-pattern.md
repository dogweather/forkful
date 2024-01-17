---
title:                "Raderar tecken som matchar ett mönster"
html_title:           "Clojure: Raderar tecken som matchar ett mönster"
simple_title:         "Raderar tecken som matchar ett mönster"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ta bort tecken som matchar ett mönster är en viktig del av programmering för att förändra och manipulera textdata. Detta gör det möjligt för programmerare att rensa och filtrera data på ett effektivt sätt.

## Så här:

```Clojure
; Ta bort alla siffror från en sträng
(re-seq #"\d" "abc123def") ; => ("1" "2" "3")

; Ta bort alla stora bokstäver från en sträng
(re-seq #"[A-Z]" "AbCdEf") ; => ("A" "C" "E")

; Ta bort alla specialtecken från en sträng
(re-seq #"[^A-Za-z0-9]" "Hello, world!") ; => ("," " ")
```

## Djupt dyk:

Att ta bort tecken som matchar ett visst mönster är ett vanligt problem som har funnits i programmering sedan lång tid tillbaka. Tidigare var det vanligt att använda inbyggda metoder i programmeringsspråk, men med Clojure är det enkelt att använda reguljära uttryck för att lösa detta.

En alternativ metod för att ta bort tecken som matchar ett mönster är att loopa igenom strängen tecken för tecken och ta bort de som matchar mönstret. Detta är dock inte lika effektivt som att använda reguljära uttryck.

Att använda reguljära uttryck för att ta bort tecken som matchar ett mönster kan också göras på andra sätt, men Clojure:s inbyggda metoder är enkla och kraftfulla nog för att hantera de flesta fall.

## Se också:

- [Clojure regex karta](https://clojure.org/reference/special_forms#special-forms-3): En översikt av regular expressions i Clojure
- [Regular expressions tutorial](https://www.regular-expressions.info/tutorial.html): En detaljerad guide om hur reguljära uttryck fungerar.