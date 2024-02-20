---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:51.711885-07:00
description: "Het genereren van willekeurige getallen in programmeren gaat over het\
  \ cre\xEBren van waarden die van tevoren niet logisch voorspeld kunnen worden.\u2026"
lastmod: 2024-02-19 22:05:09.503694
model: gpt-4-0125-preview
summary: "Het genereren van willekeurige getallen in programmeren gaat over het cre\xEB\
  ren van waarden die van tevoren niet logisch voorspeld kunnen worden.\u2026"
title: Willekeurige getallen genereren
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in programmeren gaat over het creëren van waarden die van tevoren niet logisch voorspeld kunnen worden. Programmeurs doen dit om verschillende redenen, waaronder het genereren van unieke identificatoren, het simuleren van scenario's bij spelontwikkeling of het selecteren van willekeurige monsters uit data voor analyse.

## Hoe:

In Clojure is het genereren van willekeurige getallen eenvoudig, en er zijn een paar ingebouwde functies die meteen gebruikt kunnen worden.

Om een willekeurig zwevendekommagetal tussen 0 (inclusief) en 1 (exclusief) te genereren, kunt u de `rand` functie gebruiken:

```Clojure
(rand)
;; Voorbeelduitvoer: 0.7094245047062917
```

Als u een geheel getal binnen een specifiek bereik nodig heeft, gebruik dan `rand-int`:

```Clojure
(rand-int 10)
;; Voorbeelduitvoer: 7
```

Dit geeft u een willekeurig geheel getal tussen 0 (inclusief) en het nummer dat u als argument doorgeeft (exclusief).

Voor het genereren van een willekeurig getal binnen een specifiek bereik (niet beperkt tot gehele getallen), kunt u `rand` combineren met rekenkundige bewerkingen:

```Clojure
(defn rand-range [min max]
  (+ min (* (rand) (- max min))))
;; Gebruik
(rand-range 10 20)
;; Voorbeelduitvoer: 14.857457734992847
```

Deze functie `rand-range` zal een willekeurig zwevendekommagetal teruggeven tussen de `min` en `max` waarden die u opgeeft.

Voor scenario's die meer complexe distributies of reeksen van willekeurige getallen vereisen waar herhaalbaarheid noodzakelijk is (gebruikmakend van zaden), moet u mogelijk kijken naar aanvullende bibliotheken die verder gaan dan wat ingebouwd is.

## Diepduiken

Het onderliggende mechanisme voor het genereren van willekeurige getallen in de meeste programmeertalen, inclusief Clojure, vertrouwt doorgaans op een pseudo-willekeurige getallengenerator (PRNG). Een PRNG gebruikt een algoritme om een reeks getallen te produceren die de eigenschappen van willekeurige getallen benadert. Het is de moeite waard om op te merken dat, omdat deze algoritmisch gegenereerd worden, ze niet echt willekeurig zijn, maar wel voldoende kunnen zijn voor de meeste praktische doeleinden.

In de begindagen van de computerwetenschap was het genereren van hoogwaardige willekeurige getallen een aanzienlijke uitdaging, wat leidde tot de ontwikkeling van verschillende algoritmen om willekeurigheid en distributie te verbeteren. Voor Clojure zijn de ingebouwde functies, zoals `rand` en `rand-int`, handig voor dagelijks gebruik en dekken ze een breed spectrum van algemene gebruikssituaties.

Echter, voor toepassingen die cryptografische beveiliging of complexere statistische steekproefmethoden vereisen, wenden Clojure-ontwikkelaars zich vaak tot externe bibliotheken die robuustere en gespecialiseerdere PRNG's bieden. Bibliotheken zoals `clj-random` bieden toegang tot een diversere reeks algoritmen en grotere controle over zaaien, wat cruciaal kan zijn voor simulaties, cryptografische toepassingen of elk domein waar de kwaliteit en voorspelbaarheid van de reeks willekeurige getallen significante implicaties kunnen hebben.

Hoewel de ingebouwde mogelijkheden van Clojure voor het genereren van willekeurige getallen adequaat zijn voor veel taken, kan het verkennen van externe bibliotheken diepere inzichten en opties bieden voor op maat gemaakte of kritiekere toepassingen.
