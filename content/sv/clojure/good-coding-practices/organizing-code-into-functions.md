---
title:                "Att organisera kod i funktioner"
date:                  2024-01-26T01:09:28.447292-07:00
model:                 gpt-4-1106-preview
simple_title:         "Att organisera kod i funktioner"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att organisera kod i funktioner handlar om att paketera kodblock som utför specifika uppgifter. Genom att göra detta blir koden renare, lättare att underhålla, och en vind för andra utvecklare att läsa.

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
