---
title:                "Einsatz eines Debuggers"
aliases: - /de/clojure/using-a-debugger.md
date:                  2024-01-26T03:48:19.590829-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einsatz eines Debuggers"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/using-a-debugger.md"
---

{{< edit_this_page >}}

## Was & Warum?
Einen Debugger zu verwenden bedeutet, sich selbst mit einer Lupe auszustatten, um den eigenen Code genau unter die Lupe zu nehmen. Programmierer tun dies, um Bugs zu beseitigen, den Ablauf zu verstehen und sicherzustellen, dass ihre Logik wie erwartet funktioniert.

## Wie:
Clojure stützt sich auf die Java Virtual Machine (JVM), daher erfolgt ein Großteil des Debuggings mit Java-Tools. Ein solches Werkzeug ist `CIDER`, ein leistungsstarkes Paket für die Clojure-Entwicklung in Emacs, das solide Debugging-Fähigkeiten bietet. Lassen Sie uns einsteigen:

```clojure
;; Zuerst in ein Clojure-Projekt innerhalb von Emacs mittels CIDER einsteigen
M-x cider-jack-in

;; Einen Haltepunkt setzen
;; Navigieren Sie zu der Zeile in Ihrem Clojure-Code, die Sie untersuchen möchten und
;; drücken Sie "C-c M-b" oder führen Sie aus:
M-x cider-debug-defun-at-point

;; Wenn der Code ausgeführt wird, erreichen Sie den Haltepunkt. CIDER wird Sie auffordern mit:
;; 1. n, um zum nächsten logischen Schritt in der Ausführung zu gehen,
;; 2. c, um die Ausführung bis zum nächsten Haltepunkt fortzusetzen,
;; 3. q, um das Debugging zu beenden.

;; Lokale Variablen am Haltepunkt inspizieren
;; Während Sie an einem Haltepunkt sind, tippen Sie:
locals

;; Sie werden eine Liste von lokalen Variablen und deren Werten sehen, die im Minibuffer gedruckt werden.
```
Eine Ausgabe könnte so aussehen:
```clojure
{:x 10, :y 20, :result 200}
```

## Tiefergehend
Der Debugger ist ein Werkzeug, das in der Computertechnik so alt wie die Hügel ist. Der Begriff "Bug" wurde in den frühen Tagen des Computings geprägt, als ein tatsächliches Insekt einen Fehler verursachte, indem es einen Kurzschluss in einer Maschine verursachte.

Obwohl `CIDER` großartig für Emacs-Enthusiasten ist, gibt es Alternativen für das Debuggen von Clojure. Beispielsweise kann die Verwendung von IntelliJ mit dem Cursive-Plugin ein GUI-orientiertes Debugging-Erlebnis bieten. Außerdem können Sie das integrierte Leiningen oder tools.deps nutzen, um den Prozessablauf beim Debuggen zu steuern.

Unter der Haube manipulieren diese Debugger oft Bytecodes, führen Auswertungen in dedizierten nREPL-Sitzungen durch und bieten Stack Trace-Inspektion. Sie nutzen die Fähigkeiten der zugrunde liegenden JVM, indem sie auf den Reichtum von Javas Debugging-Frameworks zurückgreifen.

## Siehe auch
- [CIDER Debugger Dokumentation](https://docs.cider.mx/cider/debugging/debugger.html)
- [Cursive Debugger](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen für Automatisierung und Debugging](https://leiningen.org/)
- [tools.deps.alpha für mehr Kontrolle](https://github.com/clojure/tools.deps.alpha)
