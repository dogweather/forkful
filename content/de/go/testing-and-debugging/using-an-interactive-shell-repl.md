---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:09.841480-07:00
description: "Eine interaktive Shell oder eine Read-Eval-Print-Schleife (REPL) erm\xF6\
  glicht es Ihnen, in Echtzeit mit Go-Code zu experimentieren, Befehle auszuf\xFC\
  hren und\u2026"
lastmod: '2024-03-13T22:44:53.289403-06:00'
model: gpt-4-0125-preview
summary: "Eine interaktive Shell oder eine Read-Eval-Print-Schleife (REPL) erm\xF6\
  glicht es Ihnen, in Echtzeit mit Go-Code zu experimentieren, Befehle auszuf\xFC\
  hren und\u2026"
title: Verwendung einer interaktiven Shell (REPL)
weight: 34
---

## Was & Warum?

Eine interaktive Shell oder eine Read-Eval-Print-Schleife (REPL) ermöglicht es Ihnen, in Echtzeit mit Go-Code zu experimentieren, Befehle auszuführen und sofortiges Feedback zu erhalten. Dieser Ansatz wird häufig zum Lernen, Debuggen und Prototyping verwendet, da er den traditionellen Zyklus von Bearbeiten-Kompilieren-Ausführen umgeht, was den Entwicklungsprozess schneller und intuitiver macht.

## Wie geht das:

Obwohl Go keine eingebaute REPL enthält, hat die Community Tools wie `gore` entwickelt, um diese Lücke zu schließen. Installieren Sie zuerst `gore`, indem Sie folgenden Befehl ausführen:

```
$ go get -u github.com/motemen/gore
```

Nach der Installation starten Sie `gore`, indem Sie `gore` in Ihrem Terminal eingeben:

```
$ gore
```

Sie sollten jetzt eine Aufforderung sehen, die bereit ist, Go-Befehle zu akzeptieren. Versuchen wir ein einfaches Beispiel:

```
gore> :import fmt
gore> fmt.Println("Hallo, Go REPL!")
```

Sie würden eine Ausgabe wie folgt sehen:

```
Hallo, Go REPL!
```

Variablen und Funktionsdefinitionen funktionieren wie erwartet. Sie können eine Funktion deklarieren:

```
gore> :import math
gore> areaCircle := func(radius float64) float64 {
...> return math.Pi * radius * radius
...> }
gore> fmt.Println("Fläche eines Kreises mit Radius 4:", areaCircle(4))
```

Und erhalten sofort die Ausgabe:

```
Fläche eines Kreises mit Radius 4: 50.26548245743669
```

## Tiefergehender Einblick:

Das Konzept einer REPL ist alt und geht zurück auf die Lisp-Maschinen der 1960er Jahre, die ein interaktives Programmiererlebnis boten. Anders als Sprachen wie Python oder JavaScript wurde Go ohne REPL entworfen, wobei der Fokus stattdessen auf kompilierten Binärdateien für Leistung und Einfachheit liegt. Dies spiegelt die Philosophie der Einfachheit von Go wider und sein Design für skalierbare und wartbare Software.

Dennoch zeigen Tools wie `gore` oder `goplay` die Ressourcenstärke der Go-Community, um diese Lücke zu überbrücken. Diese Tools parsen Go-Code dynamisch und verwenden das Paket `go/eval` oder ähnliche Mechanismen, um ihn in Echtzeit auszuführen, allerdings mit einigen Einschränkungen im Vergleich zu einer nativen REPL-Umgebung. Diese Einschränkungen resultieren aus dem Typsystem und dem Kompilierungsmodell von Go, welche eine on-the-fly Evaluation herausfordernd machen können.

Obwohl REPL-Umgebungen außerordentlich nützlich für die Bildung und schnelle Tests sind, neigt das Go-Ökosystem typischerweise zu den traditionellen Prozessen von Kompilieren und Ausführen für die meisten Entwicklungsaufgaben. IDEs und Editoren mit Go-Unterstützung, wie Visual Studio Code oder GoLand, bieten integrierte Werkzeuge für Tests und Debugging, die den Bedarf an einer REPL für die professionelle Entwicklung weitgehend verringern.

Für explorative Programmierung, Prototyping oder Lernen bieten allerdings REPLs wie `gore` eine wertvolle Alternative, die es Programmierern, die an REPLs in anderen Sprachen gewöhnt sind, ermöglicht, eine ähnliche Erfahrung in Go zu genießen.
