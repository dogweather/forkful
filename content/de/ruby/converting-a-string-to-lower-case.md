---
title:    "Ruby: Ein String in Kleinbuchstaben umwandeln"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie ein erfahrener Ruby-Programmierer sind, wissen Sie vielleicht bereits, wie man eine Zeichenfolge in Kleinbuchstaben umwandelt. Aber für diejenigen, die neu in der Programmiersprache sind, kann dieser Vorgang etwas verwirrend sein. Deshalb möchte ich Ihnen in diesem Blog-Beitrag zeigen, warum es wichtig ist, Strings in Kleinbuchstaben umzuwandeln und wie Sie dies in Ruby tun können.

## So geht's

Um eine Zeichenfolge in Kleinbuchstaben umzuwandeln, verwenden wir die `downcase` Methode. Sehen wir uns ein Beispiel an:

```Ruby
string = "HALLO WELT"
puts string.downcase
```

Dieser Code gibt `"hallo welt"` als Output aus. Wie Sie sehen, wurden alle Großbuchstaben in der Zeichenfolge in Kleinbuchstaben umgewandelt. Das kann besonders nützlich sein, wenn Sie mit Benutzereingaben arbeiten oder Text für die Ausgabe formatieren möchten.

Um das Ganze noch besser zu verstehen, lassen Sie uns ein weiteres Beispiel betrachten, bei dem wir die `downcase!` Methode verwenden. Diese Methode ändert den ursprünglichen Wert der Variablen, anstatt einen neuen Wert zurückzugeben.

```Ruby
string = "Hallo Welt!"
string.downcase!
puts string
```

Der Output für dieses Beispiel ist `hallo welt!`. Beachten Sie, dass wir hier keine `puts` Methode verwenden, da die Variable bereits den neuen Wert enthält.

## Tiefergehende Einblicke

Um Strings in Kleinbuchstaben umzuwandeln, verwendet Ruby intern die ASCII-Tabelle. Dies ist eine Tabelle, die jedem Zeichen eine numerische Darstellung zuweist. Großbuchstaben haben eine höhere numerische Darstellung als Kleinbuchstaben, daher wird durch die Anwendung von `downcase` die numerische Darstellung jedes Zeichens verringert. Dies ist jedoch nicht der einzige Weg, um Strings in Ruby zu konvertieren. Es gibt auch die `capitalize` und `swapcase` Methoden, die jeweils andere Ergebnisse erzielen.

## Siehe auch

- [Ruby-Dokumentation zur downcase Methode](https://ruby-doc.org/core-2.5.1/String.html#method-i-downcase)
- [ASCII-Tabelle](https://www.ascii-code.com/)
- [Weitere String-Manipulationsmethoden in Ruby](https://www.rubyguides.com/2019/02/ruby-string-methods/)