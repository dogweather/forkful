---
title:                "Logboekregistratie"
aliases: - /nl/clojure/logging.md
date:                  2024-01-28T22:03:03.938405-07:00
model:                 gpt-4-0125-preview
simple_title:         "Logboekregistratie"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Loggen is in wezen de software-equivalent van een scheepsjournaal; het is een manier om gebeurtenissen vast te leggen die plaatsvinden terwijl een applicatie wordt uitgevoerd. Programmeurs doen dit om deze gebeurtenissen bij te houden voor debugging, audit trails, of om inzichten te verkrijgen in het gedrag van een systeem in productie.

## Hoe te:
Clojure leunt op Java's logvoorzieningen, maar je kunt er op een meer idiomatische Clojure-manier toegang toe krijgen. Laten we eens kijken hoe je `clojure.tools.logging` zou kunnen gebruiken, dat een eenvoudige abstractie biedt over verschillende logframeworks:

Voeg eerst een afhankelijkheid voor `clojure.tools.logging` en een logimplementatie zoals `log4j` toe in je `project.clj`:

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

Laten we nu wat berichten loggen:

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "Begin met intense berekening...")
  (Thread/sleep 3000) ; Een lange berekening simuleren
  (log/info "Berekening voltooid. Het antwoord is 42.")
  42)

(compute-answer-to-everything)
```
De output toont standaard geen `DEBUG`-berichten, aangezien logniveaus doorgaans zijn ingesteld op `INFO`:

```
INFO  [jouw-namespace] - Berekening voltooid. Het antwoord is 42.
```

Je kunt de logniveaus en appenders configureren in een `log4j.properties`-bestand om indien nodig meer gedetailleerde output te krijgen.

## Diepere duik
Clojure's `clojure.tools.logging` bestaat al een tijdje en fungeert als een brug tussen de Clojure-code en de Java-logwereld. Historisch gezien heeft Java verschillende iteraties en bibliotheken voor loggen doorgemaakt, zoals de ingebouwde log-API van Java, `log4j`, `slf4j` en `logback`.

In Clojure, terwijl je direct Java's logframeworks kunt gebruiken, detecteert `clojure.tools.logging` en delegeren naar welk logframework het ook in je classpath vindt, wat je bespaart van strak gekoppeld zijn aan een specifieke implementatie. Dit kan helpen om je Clojure-code draagbaarder en modulairder te houden.

Alternatieven voor `clojure.tools.logging` binnen het Clojure-ecosysteem omvatten bibliotheken zoals `timbre`, wat een puur Clojure-logbibliotheek is met functies zoals logrotatie, filtering en asynchroon loggen direct uit de doos.

Implementatiedetails zijn cruciaal als het gaat om loggen in een multi-threaded omgeving zoals Clojure. Hier bieden onveranderlijkheid en zij-effectbeheer duidelijke voordelen. Loggen, als een zij-effect, moet zorgvuldig worden behandeld om prestatieknelpunten te voorkomen en thread-veiligheid te garanderen, wat de meeste Java-logframeworks al verzorgen.

Denk ten slotte aan gestructureerd loggen, waarbij logs worden geschreven als gestructureerde gegevens (zoals JSON). Dit kan uitermate nuttig zijn voor latere analyse en verwerking, vooral bij het omgaan met grootschalige gedistribueerde systemen.

## Zie ook
Als je honger hebt naar meer, overweeg dan deze bronnen te bekijken:

- Clojure Tools Logging documentatie: https://github.com/clojure/tools.logging
- Timbre, een Clojure logbibliotheek: https://github.com/ptaoussanis/timbre
- Configureren van Log4J in Clojure: http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- Logback Handleiding voor geavanceerde setups: http://logback.qos.ch/manual/
- Een gids over gestructureerd loggen in Clojure: https://corfield.org/blog/2020/04/28/structured-logging/
