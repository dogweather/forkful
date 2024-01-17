---
title:                "Interpolering av en sträng"
html_title:           "Clojure: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi pratar om interpolering av strängar i Clojure, menar vi egentligen att vi vill skapa nya strängar genom att kombinera variabler eller uttryck med text. Detta kan vara användbart för att skapa dynamiska meddelanden eller för att enklare formatera texten i vår kod.

## Så här gör du:
```Clojure
(def name "Lisa")
(def age 30)

(str "Hej! Mitt namn är " name " och jag är " age " år gammal.")
; Output: "Hej! Mitt namn är Lisa och jag är 30 år gammal."
```

Vi använder funktionen `str` för att skapa en ny sträng där vi interpolerar värdet av våra variabler `name` och `age` tillsammans med statisk text.

Man kan också använda sig av `format` för att skapa mer strukturerade meddelanden med hjälp av specifika formatmallar. I exemplet nedan använder vi `%s` för att ersätta med vår strängvariabel `name` och `%d` för att ersätta med vårt helatal `age`.

```Clojure
(format "Hej! Mitt namn är %s och jag är %d år gammal." name age)
; Output: "Hej! Mitt namn är Lisa och jag är 30 år gammal."
```

## Djupdykning
Att interpolera strängar har funnits länge inom programmering och används ofta för att skapa dynamiska meddelanden eller strukturerade texter som till exempel loggar eller databasfrågor. I Clojure finns det flera olika sätt att interpolera strängar beroende på vad som passar bäst för vårt specifika behov.

Det finns också alternativ till `str` och `format` såsom `pr-str` som returnerar en formaterad sträng baserat på vad som utvärderas av `print`, eller `prn-str` som gör det samma fast med `println`. Det är värt att experimentera med olika funktioner för att hitta den som passar vår kod bäst.

Implementationen av interpolering av strängar i Clojure kan variera beroende på vilken version eller implementation man använder. Det kan vara värt att kolla upp dokumentationen för den specifika versionen om man stöter på problem eller vill veta mer om hur det fungerar under huven.

## Se även
Clojure-dokumentationen för `str` och `format`: https://clojuredocs.org/clojure.core/str
Clojure-dokumentationen för `pr-str` och `prn-str`: https://clojuredocs.org/clojure.core/pr-str