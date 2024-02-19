---
aliases:
- /nl/clojure/using-an-interactive-shell-repl/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:21.112275-07:00
description: "REPL, oftewel Read-Eval-Print Loop, is een programmeeromgeving voor\
  \ het dynamisch testen van Clojure-code stuk voor stuk. Programmeurs gebruiken het\
  \ voor\u2026"
lastmod: 2024-02-18 23:09:01.481427
model: gpt-4-0125-preview
summary: "REPL, oftewel Read-Eval-Print Loop, is een programmeeromgeving voor het\
  \ dynamisch testen van Clojure-code stuk voor stuk. Programmeurs gebruiken het voor\u2026"
title: Het gebruik van een interactieve shell (REPL)
---

{{< edit_this_page >}}

## Wat & Waarom?
REPL, oftewel Read-Eval-Print Loop, is een programmeeromgeving voor het dynamisch testen van Clojure-code stuk voor stuk. Programmeurs gebruiken het voor onmiddellijke feedback, iteratieve ontwikkeling en snelle experimenten zonder de overhead van het compileren of het opzetten van een complete projectomgeving.

## Hoe te:
Begin met het opstarten van REPL:

```Clojure
user=> (println "Hallo, REPL!")
Hallo, REPL!
nil
```

Definieer een functie en probeer het uit:
```Clojure
user=> (defn greet [name] (str "Hallo, " name "!"))
#'user/greet
user=> (greet "Clojure Programmeur")
"Hallo, Clojure Programmeur!"
```

Experimenteer met datastructuren:
```Clojure
user=> (def my-map {:a 1 :b 2})
#'user/my-map
user=> (assoc my-map :c 3)
{:a 1, :b 2, :c 3}
```

## Diepgaande duik
De REPL is cruciaal voor de filosofie van interactieve ontwikkeling van de Lisp-familie, en Clojure, een moderne Lisp-dialect, maakt geweldig gebruik van deze tool. Het dateert uit de eerste Lisp REPL in de late jaren 1950. Alternatieven in andere talen omvatten Python's interpreter en de console van Node.js, maar Clojure's REPL heeft een eersteklas status en is integraal onderdeel van de workflow.

Een Clojure REPL-sessie kan worden geïntegreerd in verschillende omgevingen zoals de command-line, IDE's (zoals IntelliJ met Cursive, of Emacs met CIDER), of browsergebaseerde tools zoals Nightcode. In een diepere zin stelt de REPL de ontwikkelaar in staat om de constructies van de taal in real-time te manipuleren en staten over verschillende transformaties te dragen, wat vaak leidt tot verkennend programmeren en robuustere code.

De functionaliteit van de REPL schittert met tools zoals `lein repl` of `clj`, die mogelijkheden bieden voor afhankelijkheidsbeheer, diverse plug-ins, en projectspecifieke aanpassingen, wat leidt tot een productiever en flexibeler ontwikkelingsproces.

## Zie ook
- De officiële Clojure websitegids over de REPL: https://clojure.org/guides/repl/introduction
- Rich Hickey's praatje over REPL-gedreven ontwikkeling: https://www.youtube.com/watch?v=Qx0-pViyIDU
- Praktisch Clojure: het gebruik van de REPL voor iteratieve ontwikkeling: http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html
