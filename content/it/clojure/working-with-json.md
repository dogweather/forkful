---
title:                "Lavorare con json"
html_title:           "Clojure: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Cosa & perché?

Lavorare con JSON significa manipolare dati in formato JSON (JavaScript Object Notation), un formato di dati molto comune per lo scambio di informazioni tra applicazioni. I programmatori spesso utilizzano JSON per la sua semplicità e leggibilità, rendendolo ideale per la comunicazione tra sistemi diversi.

## Come fare:

```Clojure
;; Convertire un dato in formato JSON in una stringa
(json/generate-string {:name "Mario" :age 30})

Output: "{\"name\":\"Mario\",\"age\":30}"

;; Convertire una stringa JSON in un hashmap
(json/parse-string "{\"name\":\"Mario\",\"age\":30}")

Output: {:name "Mario", :age 30}
```

## Approfondimento:

JSON è stato originariamente ideato da Douglas Crockford nel 2001 ed è diventato uno standard del web grazie alla sua compatibilità con JavaScript. Esistono diversi altri formati di dati utilizzati per lo scambio di informazioni, tra cui XML e CSV, ma JSON è diventato il formato preferito per molti programmatori grazie alla sua leggibilità e alla sua struttura semplice. In Clojure, esistono anche alcune librerie alternative per lavorare con JSON come Cheshire e data.json.

## Vedi anche:

https://www.w3schools.com/js/js_json_intro.asp
https://www.json.org/json-it.html
https://github.com/clojure/data.json