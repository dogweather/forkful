---
title:                "Clojure: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Varför använda sig av reguljära uttryck?

När man arbetar med textsträngar i Clojure är det ofta användbart att kunna söka och manipulera dem med hjälp av reguljära uttryck. Detta är ett kraftfullt verktyg som kan användas för att matcha och hitta mönster i texten, vilket sparar både tid och ansträngning.

## Hur man använder reguljära uttryck i Clojure

För att använda reguljära uttryck i Clojure behöver man först importera biblioteket "java.util.regex". Sedan kan man använda funktionen "re-seq" för att söka efter mönster i en textsträng.

```Clojure
(import 'java.util.regex.Pattern)

(re-seq #"hej" "Hej, hur mår du?") ; => ("hej")
(re-seq #"c.l" "cool, coal, cul, cup") ; => ("col" "cul")
```

I dessa exempel använder vi "#" före vårt reguljära uttryck för att indikera att vi vill söka efter exakt det mönster vi har specificerat. Om vi vill söka efter ett mönster som kan förekomma flera gånger i en textsträng kan vi använda "*" istället.

```Clojure
(re-seq #"hej*" "Hej, hur mår du?") ; => ("hej")
(re-seq #"c*l" "cool, coal, cul, cup") ; => ("cool" "coal" "cul")
```

Man kan också använda reguljära uttryck för att ersätta delar av en textsträng med annan text. Detta görs med funktionen "re-substitutions".

```Clojure
(re-substitution #"k[aä]tt" "hund" "Min katt heter Misse") ; => "Min hund heter Misse"
```

## Djupdykning i reguljära uttryck

Reguljära uttryck är mycket kraftfulla och komplexa, så det finns många fler mönster och funktioner att utforska. Man kan till exempel använda "grupper" i sina reguljära uttryck för att hitta, extrahera och ersätta specifika delar av en textsträng. Man kan också använda "kvantifikatorer" för att specificera hur många gånger ett mönster ska matcha.

Det finns mycket information och resurser tillgängliga på internet för att lära sig mer om reguljära uttryck i Clojure. Det är också en bra idé att experimentera med olika mönster och funktioner för att bli mer bekväm med dem.

## Se även

- [Officiell Clojure Regex-dokumentation](https://clojure.github.io/clojure/clojure.java-time-api.html#clojure.java-time.instant)
- [Regex Cheat Sheet for Clojure](https://medium.com/@wisecobbler/regex-cheatsheet-for-clojure-1d31e4b6f840)
- [Clojure för Nybörjare: Reguljära Uttryck](https://clojure.org/guides/learn/regular_expressions)