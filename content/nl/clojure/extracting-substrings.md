---
title:                "Substrings extraheren"
date:                  2024-01-28T21:59:44.827424-07:00
model:                 gpt-4-0125-preview
simple_title:         "Substrings extraheren"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het extraheren van substrings betekent het grijpen van een specifiek deel van een string—zoals het eruit halen van de goede stukjes van een sandwich. Programmeurs doen dit om gegevens te isoleren, invoer te valideren, of simpelweg omdat we slechts dat stukje nodig hebben en niet de hele tekstblob.

## Hoe te:
Clojure maakt het makkelijk om met strings te werken. Voor het extraheren van substrings is `subs` je go-to functie:

```clojure
(let [tekst "ClojureRocks"]
  (subs tekst 7)) ; => "Rocks"

(let [tekst "ClojureRocks"]
  (subs tekst 0 7)) ; => "Clojure"
```

En dat is het—geef het een startindex, en optioneel een eindindex, en je zal de string precies hakken zoals je het nodig hebt.

## Diepe Duik
Het extraheren van substrings is niet nieuw—bestaat al sinds de vroege dagen van programmeren. In Clojure is `subs` een eenvoudige functie. Het maakt deel uit van Clojure's Java interop mogelijkheden, liftend op Java's `substring` methode. Twee belangrijke punten: negatieve indices zijn niet toegestaan, en het is op nul gebaseerd (begint te tellen bij nul). Dus onthoud dat of je zult er één naast zitten.

Alternatieven? Zeker. Regex met `re-find` en `re-matcher` voor complexe patronen, of `split` als je deelt bij een delimiter. Elk hulpmiddel heeft zijn plaats, maar niets verslaat `subs` voor eenvoudigheid.

Wat betreft de implementatie, `subs` kopieert geen karakters, het deelt de oorspronkelijke string's character array. Efficiënt, maar als je oorspronkelijke string enorm is en je slechts een klein beetje nodig hebt, kun je onbedoeld de hele grote string in het geheugen houden.

## Zie Ook:
- Officiële Clojure String API: [clojure.string](https://clojuredocs.org/clojure.string)
- Java `substring`: Omdat dat de krachtpatser achter `subs` is. [Java substring](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int))
- Reguliere expressies in Clojure: [re-find](https://clojuredocs.org/clojure.core/re-find)
- Strings splitsen in Clojure: [split](https://clojuredocs.org/clojure.string/split)
