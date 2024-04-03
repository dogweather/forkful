---
date: 2024-01-26 01:09:28.447292-07:00
description: "Hur man g\xF6r: Clojure-funktioner definieras med `defn`, f\xF6ljt av\
  \ ett namn, parametrar och en kropp. H\xE4r \xE4r ett snabbt exempel."
lastmod: '2024-03-13T22:44:37.530800-06:00'
model: gpt-4-1106-preview
summary: "Clojure-funktioner definieras med `defn`, f\xF6ljt av ett namn, parametrar\
  \ och en kropp."
title: Att organisera kod i funktioner
weight: 18
---

## Hur man gör:
Clojure-funktioner definieras med `defn`, följt av ett namn, parametrar och en kropp. Här är ett snabbt exempel.

```Clojure
(defn greet [name]
  (str "Hej, " name "!"))

(greet "Alex") ; => "Hej, Alex!"
```

Låt oss säga att vi vill beräkna arean på en rektangel. Istället för att klumpa ihop allt separat, delar vi upp det i två funktioner:

```Clojure
(defn area [length width]
  (* length width))

(defn print-area [length width]
  (println "Arean är:" (area length width)))

(print-area 3 4) ; => Arean är: 12
```

## Djupdykning
Förr i tiden skulle kodare bara mosa in all sin logik i en enda block. Det var fult. Sedan kom strukturerad programmering längs, och funktioner blev en grej. I Clojure är varje funktion förstaklassig - du kan slunga dem omkring som vilket annat värde som helst.

Alternativ? Vissa kanske leker med multimetoder eller funktioner av högre ordning, men de är bara kryddor i funktionsgrytan.

Allt i en funktions detaljer: de är oföränderliga i Clojure, vilket gör att sidoeffekter blir mindre sannolika. De lutar sig tungt på rekursion istället för typiska loopar, vilket passar väl med språkets funktionella paradigmer.

## Se även
- Clojures egen guide: https://clojure.org/guides/learn/functions
- Grundläggande funktionsprogrammering: https://www.braveclojure.com/core-functions-in-depth/
- Rich Hickeys föredrag: https://changelog.com/posts/rich-hickeys-greatest-hits - för insikt i Clojures filosofi.
