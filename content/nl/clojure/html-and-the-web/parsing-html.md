---
title:                "HTML Parsen"
aliases:
- /nl/clojure/parsing-html.md
date:                  2024-01-28T22:03:38.535061-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML Parsen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

HTML-parseren is het proces van het omzetten van een HTML-string naar een datastructuur die je programma kan begrijpen en manipuleren. Programmeurs doen dit om te interageren met, inhoud van het web te extraheren, en te wijzigen.

## Hoe:

Om HTML in Clojure te parsen, gebruiken we de `clj-tagsoup` bibliotheek, een wrapper voor de Tagsoup Java-bibliotheek die handig is voor het parsen van real-world HTML.

Voeg eerst de clj-tagsoup-afhankelijkheid toe aan je project:

```clojure
[clj-tagsoup "0.3.3"] ; Controleer op de nieuwste versie
```

Nu, laten we wat HTML parsen:

```clojure
(require '[clj-tagsoup.core :as tagsoup])

; Parseer HTML en krijg een vector van mappen die de geparseerde elementen voorstellen
(def parsed-html (tagsoup/parse-string "<html><body><p>Hallo, Wereld!</p></body></html>"))

; Toegang tot elementen
(println (first parsed-html))
```

Voorbeelduitvoer:

```clojure
{:tag :html, :attrs {}, :content [...]}
```

Om specifieke elementen te extraheren, zoals paragrafen:

```clojure
(defn extract-paragraphs [html]
  (let [parsed (tagsoup/parse-string html)]
    (filter #(= :p (:tag %)) parsed)))

; Gebruik
(extract-paragraphs "<p>Eerste</p><p>Tweede</p>")
```

## Diepe Duik

HTML parsen in Clojure, net als in andere talen, houdt typisch het navigeren door een boomachtige structuur in. Vroeger kon dit rommelig worden. Bibliotheken zoals Tagsoup hebben het leven makkelijker gemaakt door eigenaardige real-world HTML aan te kunnen.

De functionele aard van Clojure laat ons soepel HTML-gegevens manipuleren. Bibliotheken zoals `clj-tagsoup` benutten de beproefde hulpmiddelen van Java met een toevoeging van Clojure's elegantie.

Alternatieve bibliotheken zijn onder andere `Enlive` en `Hickory`. Enlive is gespecialiseerd in zowel het parsen als het templaten, waardoor meer complexe operaties mogelijk zijn. Hickory vertaalt HTML naar Clojure-datastructuren voor degenen die een pure Clojure-oplossing verkiezen.

De implementatie richt zich op gemak en een declaratieve stijl. Onder de motorkap gebruikt `clj-tagsoup` locators en navigators om door HTML te reizen, waardoor een hogere abstractie wordt geboden dan directe DOM-manipulatie.

## Zie Ook

- clj-tagsoup op GitHub: https://github.com/nathell/clj-tagsoup
- Tagsoup, de onderliggende Java-bibliotheek: https://github.com/McCLIM/cl-tagsoup
- Enlive, een andere Clojure HTML-parserbibliotheek: https://github.com/cgrand/enlive
- Hickory, een Clojure-project voor HTML-parseren: https://github.com/davidsantiago/hickory
