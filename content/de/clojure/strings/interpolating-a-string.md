---
date: 2024-01-20 17:50:42.707262-07:00
description: "String-Interpolation erm\xF6glicht es, Variablen oder Ausdr\xFCcke in\
  \ einem String einzubetten, sodass der String deren Werte enth\xE4lt. Programmierer\
  \ nutzen dies\u2026"
lastmod: '2024-03-13T22:44:53.406519-06:00'
model: gpt-4-1106-preview
summary: "String-Interpolation erm\xF6glicht es, Variablen oder Ausdr\xFCcke in einem\
  \ String einzubetten, sodass der String deren Werte enth\xE4lt. Programmierer nutzen\
  \ dies\u2026"
title: Zeichenketten interpolieren
weight: 8
---

## Was & Warum?
String-Interpolation ermöglicht es, Variablen oder Ausdrücke in einem String einzubetten, sodass der String deren Werte enthält. Programmierer nutzen dies für dynamisch generierte Nachrichten und die Formatierung von Ausgaben, um Code sauber und flexibel zu halten.

## How to:
Mit Clojure können wir keine direkte String-Interpolation wie in einigen anderen Sprachen verwenden, aber wir haben mächtige Alternativen.

```Clojure
;; Verwende `str` und `format`
(def name "Welt")
(str "Hallo " name "!") ; "Hallo Welt!"

;; `format` kann benutzt werden für komplexere Szenarien
(format "Hallo, %s!" name) ; "Hallo, Welt!"

;; Ein weiterer Weg ist die Verwendung von `clojure.core/strint`
(require '[clojure.core/strint :refer [<<]])
(<< "Hallo, ${name}!") ; "Hallo, Welt!"
```

Eine saubere Ausgabe:

```Clojure
;; Saubere Ausgabe mit `format`
(format "Der Preis beträgt: %.2f Euro" 19.99) ; "Der Preis beträgt: 19.99 Euro"
```

## Deep Dive:
Historisch gesehen hat Clojure keine eingebaute String-Interpolation wie zum Beispiel Python oder Ruby. Dies ist der funktionalen Natur Clojures geschuldet, die darauf ausgelegt ist, Seiteneffekte zu vermeiden und Unveränderlichkeit zu fördern.

Folgende Ansätze sind üblich in der Clojure-Welt:
- `str`: Verkettet Werte zu einem neuen String.
- `format`: Verwendet Java's `String.format` zur Formatierung. Sehr mächtig für komplexe Ausgaben.
- `clojure.core/strint`: Ermöglicht ein Template-Muster ähnlich der traditionellen String-Interpolation mit `${}`-Syntax.

Da Clojure auf der JVM läuft, sind die `str`- und `format`-Methoden letztlich Bindungen zu Java-Methoden und bieten daher eine hohe Leistung. `clojure.core/strint` ist eine community-erstellte Bibliothek, die ergänzt, was in der Standardbibliothek fehlt.

## See Also:
- Clojure's [`str`](https://clojuredocs.org/clojure.core/str) Funktion 
- [`format`](https://clojuredocs.org/clojure.core/format) in der Clojure-Dokumentation
- Die [`clojure.core/strint`](https://github.com/clojure/core.incubator) Bibliothek auf GitHub
