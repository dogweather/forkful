---
title:                "Clojure: Extrahera substrängar"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

I många programmeringsprojekt finns det behov av att hämta ut delar av en sträng, istället för att bara använda hela strängen som den är. Detta kan till exempel vara för att manipulera data eller för att skapa mer dynamiska applikationer. I Clojure finns det enkla sätt att extrahera substrings från en sträng, vilket kan vara mycket användbart i många olika scenarion.

## Hur man gör

För att extrahera en substring från en sträng i Clojure, kan man använda funktionen `subs`. Den tar in två parametrar - den ursprungliga strängen och vilka index som substringen ska hämtas från. Till exempel:

```Clojure
(def sträng "Hej, det här är en teststräng.")

(subs sträng 4 10)
```

Resultatet av detta skulle bli "det här", eftersom vi hämtar ut alla tecken från index 4 till index 10 (exklusive det sista tecknet). Detta fungerar även med negativa index, där den räknar bakifrån. Till exempel:

```Clojure
(subs sträng -9 -1)
```

Skulle resultera i "sträng" eftersom vi här hämtar ut de sista nio tecknen i strängen.

För att bara hämta en del av en sträng, kan man använda funktionen `subs` tillsammans med funktionen `str/split`. Detta skulle till exempel kunna se ut såhär:

```Clojure
(def sträng "Hejsan, detta är en annan teststräng.")

(-> sträng
    (str/split #",")
    first
    (subs 3))
```

Det här skulle returnera "s", eftersom vi först delar upp strängen vid kommatecknet, tar första delen av den uppdelade strängen och sedan hämtar ut den tredje bokstaven från den delen.

## Djupdykning

När man extraherar substrings från en sträng, är det viktigt att vara medveten om hur Clojure behandlar unicode-tecken. Om man till exempel har en sträng med emoji, kan det vara lite knepigare att extrahera en del av den. Ett sätt att göra detta är att använda funktionen `seq` för att omvandla strängen till en sekvens av tecken och sedan ta ut delar av den. Till exempel:

```Clojure
(def sträng "Hej 😊, detta är en teststräng.")

(subs (seq sträng) 4 10)
```

Det här skulle resultera i "😊, det", eftersom Clojure här ser alla tecken som individuella element i sekvensen och behandlar dem som sådana.

## Se även

* [Clojure Docs: subs](https://clojuredocs.org/clojure.core/subs)
* [Clojure Docs: seq](https://clojuredocs.org/clojure.core/seq)
* [Clojure Docs: split](https://clojuredocs.org/clojure.string/split)