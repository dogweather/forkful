---
title:                "Ausgabe von Debug-Informationen"
html_title:           "Ruby: Ausgabe von Debug-Informationen"
simple_title:         "Ausgabe von Debug-Informationen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Drucken von Debug-Ausgaben ist eine gängige Methode für Programmierer, um während des Entwicklungsprozesses Fehler zu finden und zu beheben. Es ermöglicht uns, den Ablauf unseres Codes zu verfolgen und zu überprüfen, ob die erwarteten Ergebnisse erzielt werden.

## Wie geht's?

Um Debug-Ausgaben in Ruby zu drucken, verwenden wir einfach den Befehl `puts`, gefolgt von der gewünschten Ausgabe. Wir können auch die Methode `p` verwenden, die nicht nur den Inhalt, sondern auch den Datentyp unserer Ausgabe anzeigt. Ein Beispiel:

```Ruby
puts "Hallo Welt!"
#=> Hallo Welt!

name = "Anna"
p name
#=> "Anna" (String)
```

## Tief eintauchen

Die Praxis des Druckens von Debug-Ausgaben stammt aus der Zeit, als Programmierer noch auf Papier ausgedruckte Ausgaben verwendet haben, um Fehler zu finden. Heute gibt es auch andere Möglichkeiten, wie z.B. das Debugging mit Breakpoints oder das Verwenden von Debugging-Tools.

Eine wichtige Überlegung beim Drucken von Debug-Ausgaben ist die Wahl des richtigen Ausgabekanals. In Ruby können wir auch den Standardfehlerkanal `STDERR` verwenden, um Ausgaben zu drucken, die als Fehlermeldung angezeigt werden sollen.

## Siehe auch

- [Ruby's official documentation on `puts`](https://ruby-doc.org/core-2.7.2/Kernel.html#method-i-puts)
- [A beginner's guide to debugging in Ruby](https://www.rubyguides.com/2019/06/ruby-debugging/)
- [Different ways to debug in Ruby](https://dockyard.com/blog/2016/02/10/print-debugging-in-ruby-using-printf-p-and-pp)