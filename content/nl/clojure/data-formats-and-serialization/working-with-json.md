---
title:                "Werken met JSON"
aliases:
- /nl/clojure/working-with-json/
date:                  2024-01-28T22:11:06.938422-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat en Waarom?
JSON (JavaScript Object Notation) is een gegevensformaat dat wordt gebruikt om gegevens op te slaan en te transporteren. Programmeurs gebruiken JSON vanwege de gebruiksvriendelijkheid met web-API's en het taalonafhankelijke tekstformaat, wat het erg handig maakt voor gegevensuitwisseling.

## Hoe te:
Laten we spelen met JSON in Clojure. Je hebt `Cheshire` nodig, een populaire bibliotheek voor JSON-codering/decodering.

Voeg eerst Cheshire toe aan je `project.clj` afhankelijkheden:
```clojure
[cheshire "5.10.1"]
```

JSON lezen vanuit een string en het omzetten naar een Clojure-map:
```clojure
(require '[cheshire.core :as json])

(def json-str "{\"name\":\"Clojure\"}")
(def clojure-map (json/parse-string json-str))

(println clojure-map)  ;; => {"name" "Clojure"}
```

Een Clojure-map omzetten naar een JSON-string:
```clojure
(def clojure-data {:language "Clojure" :cool true})
(def json-output (json/generate-string clojure-data))

(println json-output)  ;; => {"language":"Clojure","cool":true}
```

JSON parseren vanuit een bestand:
```clojure
(slurp "data.json")  ;; inhoud: {"message": "Hello, JSON!"}
(def file-content (slurp "data.json"))
(def message-data (json/parse-string file-content true))

(println message-data)  ;; => {"message" "Hello, JSON!"}
```

## Diepe Duik
De geschiedenis van JSON begint bij JavaScript, maar nu is het overal, niet afhankelijk van zijn oorspronkelijke taal. Alternatieven? Voorheen was XML de standaard, hoewel meer langdradig. YAML is eenvoudiger, mensvriendelijker maar niet zo universeel voor API's. Wat betreft de implementatie: Clojure is geen JavaScript, dus bibliotheken zoals Cheshire zijn essentieel. Ze overbruggen de kloof door gebruik te maken van Java-bibliotheken onder de motorkap om het parsen en genereren efficiënt te behandelen.

## Zie Ook
- [Cheshire GitHub Repo](https://github.com/dakrone/cheshire): Voor biblitheekdetails en updates.
- [JSON.org](https://www.json.org): Specificaties en details van JSON.
- [Clojure from the ground up: JSON](https://aphyr.com/posts/305-clojure-from-the-ground-up-json): Een gedetailleerde gids over het omgaan met JSON in Clojure.
