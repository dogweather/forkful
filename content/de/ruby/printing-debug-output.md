---
title:    "Ruby: Debug-Ausgabe drucken"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Das Drucken von Debug-Ausgaben ist ein wichtiger Schritt beim Programmieren in Ruby, um Fehler und Probleme im Code zu identifizieren. Es kann uns helfen, die Logik und den Fluss unseres Codes besser zu verstehen und zu überprüfen, ob unsere Erwartungen erfüllt werden.

## Wie geht das

Um Debug-Ausgaben in Ruby zu drucken, können wir die `puts` oder `p` Methode verwenden. `puts` gibt einfach den Inhalt der übergebenen Variablen oder Strings in der Konsole aus, während `p` die Ausgabe mit zusätzlichen Informationen wie dem Datentyp anzeigt. Hier ist ein Beispiel:

```Ruby
name = "Max Mustermann"
puts "Hallo, mein Name ist " + name
# Ausgabe: Hallo, mein Name ist Max Mustermann

p age = 25
# Ausgabe: 25 (mit Informationen über Datentyp)
```

## Tiefgehende Infos

Zusätzlich zu `puts` und `p` gibt es auch die `print` und `printf` Methoden, die für Debug-Ausgaben verwendet werden können. `print` gibt den Inhalt ohne Zeilenumbruch aus, während `printf` formatierte Ausgaben ermöglicht, ähnlich wie `sprintf` für Strings. Hier sind weitere Beispiele:

```Ruby
items = ["Apfel", "Banane", "Orange"]

print items
# Ausgabe: ["Apfel", "Banane", "Orange"]

printf("Ich habe %d Stück Obst.", items.length)
# Ausgabe: Ich habe 3 Stück Obst.
```

Debug-Ausgaben sind auch hilfreich, wenn wir Codeabschnitte oder Schleifen durchlaufen und überprüfen möchten, ob unsere Variablen die erwarteten Werte haben.

## Weitere Informationen

Weitere Informationen über das Drucken von Debug-Ausgaben in Ruby finden Sie in der offiziellen Ruby Dokumentation und in den folgenden Ressourcen:

- [Official Ruby Documentation](https://ruby-doc.org/core-2.7.1/IO.html#method-i-puts)
- [The Ruby Way: Debugging](https://www.rubyguides.com/2019/01/ruby-debugging/)
- [Debugging Tips and Tricks in Ruby](https://medium.com/@aliciapquintana/debugging-tips-and-tricks-in-ruby-e3093e4170a7)

## Siehe auch

- [Effektives Debugging](https://rubyonrails.org/doctrine/2015/12/02/effective_debugging_in_rails.html)
- [6 Tips for Debugging in Ruby](https://www.sitepoint.com/6-tips-debugging-ruby/)