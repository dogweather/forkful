---
title:                "Een nieuw project starten"
date:                  2024-01-28T22:08:38.145076-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een nieuw project starten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een nieuw project starten betekent het opzetten van een frisse programmeeromgeving voor je code. Programmeurs doen dit om de ontwikkeling te beginnen met een schone lei en gedachten te organiseren in tastbare code.

## Hoe te:

Om een Clojure-project op te starten, gebruiken we Leiningen, een populair bouwhulpmiddel voor Clojure:

```Clojure
;; 1. Installeer Leiningen als je dat nog niet hebt gedaan (https://leiningen.org/)
;; 2. Genereer een nieuw projectskelet:
lein new app mijn-coole-app

;; 3. Navigeer naar je nieuwe project:
cd mijn-coole-app

;; 4. Start een REPL (Read-Eval-Print Loop):
lein repl

;; Voorbeelduitvoer:
;; nREPL-server gestart op poort 12345 op host 127.0.0.1 - nrepl://127.0.0.1:12345
;; REPL-y 0.4.4, nREPL 0.6.0
;; Clojure 1.10.1
;; Java 1.8.0_232
;;     Documentatie: (doc functienaam-hier)
;;           (find-doc "deel-van-naam-hier")
;;   Bron: (source functienaam-hier)
;;  Javadoc: (javadoc java-object-of-klasse-hier)
;;     Afsluiten: Control+D of (exit) of (quit)
;;  Resultaten: Opgeslagen in vars *1, *2, *3, een uitzondering in *e

;; 5. Maak een bestand voor je code (src/mijn_coole_app/core.clj) en open het in je favoriete teksteditor.

;; 6. Schrijf wat simpele Clojure-code:
(ns mijn-coole-app.core)

(defn say-hello []
  (println "Hallo, Clojure wereld!"))

;; 7. Voer je functie uit in de REPL:
(mijn-coole-app.core/say-hello)

;; Voorbeelduitvoer:
;; Hallo, Clojure wereld!
```

## Diepgaand

Clojure-projecten beginnen vaak met Leiningen of Boot voor het beheren van afhankelijkheden, bouwen en automatiseren van taken. Leiningen bestaat sinds 2010 en is de standaardkeuze geworden voor de meeste Clojuristen.

Er bestaan alternatieve hulpmiddelen, zoals `deps.edn` en Clojure CLI-tools, die door Clojure/core zijn geïntroduceerd om een eenvoudigere beheer van afhankelijkheden en projectconfiguratie te bieden.

Clojure zelf waardeert onveranderlijkheid en functioneel programmeren. Een project correct beginnen benadrukt schoon staatbeheer en scheiding van zorgen over functies en naamruimten.

Projecten houden zich doorgaans aan een standaard directorystructuur:
- `src/` voor je hoofdcode.
- `test/` voor testcode.
- `resources/` voor niet-codebronnen.
- `project.clj` of `deps.edn` om afhankelijkheden en configuraties te beheren.

Een goede praktijk is om dingen minimaal te houden bij de start. Voeg afhankelijkheden toe naarmate je verdergaat, waardoor je project licht en beheersbaar blijft.

## Zie Ook

- [Leiningen's Startgids](https://leiningen.org/#getting-started)
- [Clojure Documentatie](https://clojuredocs.org/)
- [Clojure Stijlgids](https://guide.clojure.style/)
- [Clojure CLI-hulpmiddelen](https://clojure.org/guides/getting_started)
- [De Clojure Gereedschapskist](https://www.clojure-toolbox.com/)
