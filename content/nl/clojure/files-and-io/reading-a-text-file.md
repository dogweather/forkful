---
title:                "Een tekstbestand lezen"
date:                  2024-01-28T22:04:54.195018-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand lezen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tekstbestand lezen betekent gegevens uit een bestand op uw schijf in uw programma krijgen. Programmeurs doen dit om inhoud te verwerken of analyseren zonder handmatige invoer, taken te automatiseren of configuratiegegevens te ontleden.

## Hoe te:

```Clojure
;; Lees het hele bestand als een string
(slurp "voorbeeld.txt")

;; Uitvoer: "Hallo, dit is de inhoud van uw bestand!"

;; Lees bestand regel voor regel
(with-open [lezer (clojure.java.io/reader "voorbeeld.txt")]
  (doseq [regel (line-seq lezer)]
    (println regel)))

;; Uitvoer:
;; Hallo,
;; dit is uw
;; bestandsinhoud!
```

## Diepgaande Duik

Traditioneel was het lezen van bestanden in programmeertalen een langdradige taak met veel stappen om fouten en bronnen te beheren. In Clojure profiteert u van de `slurp` functie, een elegante one-liner om de hele inhoud van het bestand te grijpen. Voor regel-voor-regel lezen zorgt `line-seq` in combinatie met `with-open` voor efficiënte en veilige bestandshandling. Het is ook vermeldenswaard dat, hoewel `slurp` handig is, het niet ideaal is voor grote bestanden vanwege geheugenbeperkingen. Dat is wanneer `line-seq` schittert, omdat het het bestand lui leest, één regel tegelijk.

Alternatieven voor het lezen van bestanden in Clojure omvatten het gebruik van de `clojure.java.io/file` met functies zoals `reader` en constructies zoals `with-open` om de bestandshendel handmatig te beheren. De afweging hier is tussen gebruiksgemak (`slurp`) en fijnmazige controle gecombineerd met veiligheid van bronnen (`with-open` en `reader`).

Implementatiegewijs is Clojure's aanpak gegrond in Java's IO-klassen, daarom, wanneer u met bestanden in Clojure werkt, heeft u te maken met Java's volwassen, goed geteste bibliotheken, verpakt in een functioneel idioom. Houd altijd een oogje op bronnen: open bestanden verbruiken hendels en geheugen, dus nette bestandshandling is een nette gewoonte.

## Zie Ook

- ClojureDocs voor `slurp`: https://clojuredocs.org/clojure.core/slurp
- ClojureDocs voor `line-seq`: https://clojuredocs.org/clojure.core/line-seq
- Java interop in Clojure: https://clojure.org/reference/java_interop
- Werken met bestanden in Clojure (Practical.li): https://practical.li/clojure/working-with-files.html
