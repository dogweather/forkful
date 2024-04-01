---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:51.727157-07:00
description: "Een debugger gebruiken betekent dat je jezelf uitrust met een vergrootglas\
  \ om je code nauwkeurig te onderzoeken. Programmeurs doen dit om fouten te\u2026"
lastmod: '2024-03-13T22:44:50.424165-06:00'
model: gpt-4-0125-preview
summary: "Een debugger gebruiken betekent dat je jezelf uitrust met een vergrootglas\
  \ om je code nauwkeurig te onderzoeken. Programmeurs doen dit om fouten te\u2026"
title: Een debugger gebruiken
---

## Hoe:
Clojure leunt op de Java Virtuele Machine (JVM), dus veel debuggen gebeurt met Java-tools. Een dergelijke tool is `CIDER`, een krachtig pakket voor Clojure-ontwikkeling in Emacs, dat solide debugmogelijkheden heeft. Laten we erin duiken:

```clojure
;; Eerst, 'jack-in' naar een Clojure-project binnen Emacs met CIDER
M-x cider-jack-in

;; Zet een breekpunt
;; Navigeer naar de regel in je Clojure-code die je wilt inspecteren en
;; druk op "C-c M-b" of voer uit:
M-x cider-debug-defun-at-point

;; Wanneer de code loopt, kom je bij het breekpunt. CIDER zal je de volgende opties geven:
;; 1. n om naar de volgende logische stap in de uitvoering te gaan,
;; 2. c om de uitvoering voort te zetten tot het volgende breekpunt,
;; 3. q om het debuggen te stoppen.

;; Inspecteer lokale variabelen bij het breekpunt
;; Terwijl je bij een breekpunt bent, typ:
locals

;; Je ziet een lijst van lokale variabelen en hun waarden afgedrukt in de minibuffer.
```
Voorbeelduitvoer kan er zo uitzien:
```clojure
{:x 10, :y 20, :result 200}
```

## Diepere duik
De debugger is een tool zo oud als de informatica zelf. De term "bug" werd bedacht in de beginjaren van de informatica toen een daadwerkelijk insect een fout veroorzaakte door een circuit in een machine kort te sluiten.

Hoewel `CIDER` geweldig is voor Emacs-enthousiastelingen, zijn er alternatieven voor Clojure-debugging. Bijvoorbeeld, het gebruik van IntelliJ met de Cursive-plugin kan een meer GUI-gestuurde debugervaring bieden. Bovendien kun je de ingebouwde Leiningen of tools.deps gebruiken om de processtroom bij het debuggen te beheersen.

Onder de motorkap manipuleren deze debuggers vaak bytecodes, voeren evaluaties uit in speciale nREPL-sessies en bieden stapel spoor inspectie. Ze benutten de onderliggende mogelijkheden van de JVM, en tappen in op de rijkdom van Java's debugframeworks.

## Zie ook
- [CIDER Debugger Documentatie](https://docs.cider.mx/cider/debugging/debugger.html)
- [Cursive Debugger](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen voor Automatisering en Debuggen](https://leiningen.org/)
- [tools.deps.alpha voor meer controle](https://github.com/clojure/tools.deps.alpha)
