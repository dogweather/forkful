---
title:                "Arbeide med json"
html_title:           "Clojure: Arbeide med json"
simple_title:         "Arbeide med json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/working-with-json.md"
---

{{< edit_this_page >}}

# Hva er JSON og Hvorfor?

JSON (JavaScript Object Notation) er en vanlig format for datautveksling i programmering. Det er en enkel, tekstbasert måte å representere strukturert data på, som er lett å lese og skrive for både mennesker og maskiner. Programmerere bruker JSON for å sende og motta data mellom forskjellige systemer og programmer, for eksempel å hente informasjon fra en database eller kommunisere med en API.

# Hvordan:

Den enkleste måten å jobbe med JSON i Clojure er å bruke biblioteket "clojure.data.json". Dette biblioteket lar deg konvertere data mellom Clojure-strukturer og JSON-formatet. Her er et eksempel på hvordan man kan konvertere en Clojure-map til JSON og deretter tilbake til en map:

```Clojure
(require '[clojure.data.json :as json])

(def map {:navn "Jens", :alder 30})

(json/generate-string map) ;; konverterer map til JSON
;; output: "{\"navn\":\"Jens\",\"alder\":30}"
    
(json/parse-string "{\"navn\":\"Jens\",\"alder\":30}") ;; konverterer JSON til map
;; output: {:navn "Jens", :alder 30}
```

Du kan også jobbe med JSON-dokumenter som filer ved å bruke funksjonene `(json/write-str)` og `(json/read-str)`. Slik kan du enkelt lese og skrive JSON-data til og fra filer.

# Dypdykk:

JSON ble først utviklet av Douglas Crockford på slutten av 1990-tallet, og har siden blitt en populær standard for datautveksling. I tillegg til å være mye brukt i webutvikling, brukes det også i databaser, automatisering og i mobilapplikasjoner. Alternativer til JSON inkluderer XML, YAML og CSV, men JSON er generelt sett enklere å arbeide med og mer kompatibelt med moderne webteknologi.

I Clojure er det også andre biblioteker for å arbeide med JSON, som "cheshire" og "clj-json". Disse bibliotekene kan tilby forskjellige funksjoner og ytelse, så det kan være lurt å undersøke litt for å finne det som passer best for ditt prosjekt.

# Se også:

- [JSON Official Website](https://www.json.org/) - offisiell dokumentasjon og spesifikasjon for JSON
- [clojure.data.json](https://github.com/clojure/data.json) - offisiell dokumentasjon for Clojure biblioteket
- [cheshire](https://github.com/dakrone/cheshire) - et alternativt Clojure bibliotek for JSON
- [clj-json](https://github.com/mmcgrana/clj-json) - enda et alternativt Clojure bibliotek for JSON