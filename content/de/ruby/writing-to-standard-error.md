---
title:                "Ruby: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum

Das Schreiben auf den Standardfehler-Ausgabestrom ist eine wichtige Technik beim Schreiben von Ruby-Programmen. Es ermöglicht Ihnen Fehlermeldungen, Warnungen und anderen wichtigen Output anzuzeigen, die für die Fehlerbehandlung und das Debugging Ihres Codes unerlässlich sind.

# Wie geht man vor

Um auf den Standardfehler-Ausgabestrom zu schreiben, können Sie die built-in Methode `warn` verwenden. Sie akzeptiert als Argumente eine beliebige Anzahl an Strings oder Objekten und gibt diese auf dem Standardfehler-Kanal aus. Ein Beispiel:

```Ruby
warn "Dies ist eine Warnung"
warn 12345
```

Die Ausgabe sieht dann so aus:

```Ruby
# Dies ist eine Warnung
# 12345
```

# Tiefere Einblicke

Während das Schreiben auf den Standardfehler-Ausgabestrom einfach erscheint, gibt es einige wichtige Details zu beachten. Zum Beispiel, dass die `warn` Methode die Objekte in der Reihenfolge ausgibt, in der sie als Argumente übergeben wurden. Außerdem ist es wichtig zu wissen, dass die Ausgabe auf dem gleichen Kanal erfolgt wie das Standard Error Level des Prozesses.

Wenn Sie mehr über das Lesen und Schreiben auf verschiedenen Ausgabeströmen in Ruby erfahren möchten, lesen Sie bitte die offizielle Dokumentation: https://ruby-doc.org/core-2.6.3/IO.html

# Siehe auch

- Offizielle Ruby-Dokumentation über das Lesen und Schreiben von Ausgabeströmen: https://ruby-doc.org/core-2.6.3/IO.html
- Ein praktisches Beispiel für das Schreiben auf den Standardfehler-Ausgabestrom: https://www.rubyguides.com/2018/10/standard-error-ruby/
- Verwendung von `warn` vs. `puts` in Ruby: https://stackoverflow.com/questions/29837005/proper-use-of-warn-vs-puts-in-ruby