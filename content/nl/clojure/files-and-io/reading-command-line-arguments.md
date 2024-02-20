---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:57.659603-07:00
description: "Het lezen van commandoregelargumenten laat een programma informatie\
  \ rechtstreeks uit de terminalopdracht van de gebruiker halen. Programmeurs doen\
  \ dit om\u2026"
lastmod: 2024-02-19 22:05:09.524401
model: gpt-4-0125-preview
summary: "Het lezen van commandoregelargumenten laat een programma informatie rechtstreeks\
  \ uit de terminalopdracht van de gebruiker halen. Programmeurs doen dit om\u2026"
title: Commandoregelargumenten lezen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het lezen van commandoregelargumenten laat een programma informatie rechtstreeks uit de terminalopdracht van de gebruiker halen. Programmeurs doen dit om het gedrag van een programma aan te passen zonder de code zelf te wijzigen.

## Hoe:

In Clojure, pak je commandoregelargumenten met `*command-line-args*`. Hier is een eenvoudig voorbeeld:

```clojure
;; Stel je voor dat deze code in een bestand genaamd `echo.clj` staat

(defn -main [& args]
  (println "Je hebt ingevoerd:" args))

;; Om te draaien: `clojure echo.clj arg1 arg2 arg3`
```

Voorbeelduitvoer:

```
Je hebt ingevoerd: (arg1 arg2 arg3)
```

Moet je ze verwerken? Gebruik dan de collectiefuncties van Clojure.

```clojure
(defn -main [& args]
  (let [processed-args (mapv str/upper-case args)]
    (println "Hoofdletters:" processed-args)))

;; Nu zal het uitvoeren van `clojure echo.clj hallo wereld` uitvoeren:
```

Voorbeelduitvoer:

```
Hoofdletters: ["HALLO" "WERELD"]
```

## Diepere duik

De `*command-line-args*` is een var in Clojure, ingesteld op een reeks argumenten die aan het script zijn doorgegeven. Het bestaat al sinds de beginjaren van Clojure, wat aantoont dat Clojure commandoregelargumenten als eersteklas burgers behandelt.

Alternatieven? Java's mechanismen voor het grijpen van commandoregelargumenten werken ook in Clojure, dankzij interoperabiliteit. Maar dat is uitvoeriger.

Wat betreft implementatiedetails, wanneer Clojure start, analyseert het de argumenten en slaat ze op in `*command-line-args*`. Je script kan er dan wat dan ook mee doen—analyseren, negeren, transformeren, noem maar op.

## Zie ook

- Officiële Clojure CLI-tools: https://clojure.org/guides/deps_and_cli
- Clojure vanaf de basis: Commandoregel-scripting: https://aphyr.com/posts/305-clojure-from-the-ground-up-command-line
- ClojureDocs over *command-line-args*: https://clojuredocs.org/clojure.core/*command-line-args*
