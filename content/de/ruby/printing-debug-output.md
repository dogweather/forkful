---
title:                "Ruby: Ausgabe von Debug-Informationen drucken"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum?

Debugging ist ein wichtiger Teil der Programmierung und das Einbinden von Debug-Ausgaben in den Code kann dabei sehr hilfreich sein. Es ermöglicht uns, den Programmablauf zu verfolgen und zu verstehen, was im Hintergrund passiert. Dadurch können wir Fehler schneller finden und beheben.

## Wie macht man das?

Um Debug-Ausgaben in Ruby zu erstellen, können wir die `puts` Methode verwenden. Diese gibt den angegebenen String im Terminal aus. Zum Beispiel:

```ruby
puts "Hallo, Welt!"
```

Die Ausgabe des obigen Codes wird `Hallo, Welt!` sein. Dies kann uns dabei helfen, bestimmte Variablen oder Werte im Programmablauf zu überwachen.

## Tiefer in die Materie eintauchen

Neben `puts` gibt es auch die `p` Methode, die den Wert einer Variablen und deren Datenstruktur ausgibt. Zum Beispiel:

```ruby
name = "Max"
p name
```

Die Ausgabe wird `Max` inklusive der Anführungszeichen sein, was uns zeigt, dass es sich um einen String handelt. Dies kann uns dabei helfen, Variablen mit komplexen Datentypen wie Arrays oder Hashes besser zu verstehen.

Es ist auch möglich, die `puts` oder `p` Methode mit dem `inspect` Befehl zu verwenden, um die Ausgabe noch detaillierter zu gestalten. Hier ist ein Beispiel, bei dem wir das `inspect` verwendet haben:

```ruby
array = [1, 2, 3]
puts array.inspect
```

Die Ausgabe wird `[1, 2, 3]` sein, was uns zeigt, dass es sich um ein Array handelt. Wenn wir jedoch die `p` Methode verwendet hätten, wäre die Ausgabe ` [1, 2, 3]` inklusive der eckigen Klammern und Kommas gewesen.

## Siehe auch

- [Ruby-Dokumentation zu puts](https://ruby-doc.org/core-2.7.3/Kernel.html#method-i-puts)
- [Ruby-Dokumentation zu p](https://ruby-doc.org/core-2.7.3/Kernel.html#method-i-p)
- [Ruby-Dokumentation zu inspect](https://ruby-doc.org/core-2.7.3/Object.html#method-i-inspect)