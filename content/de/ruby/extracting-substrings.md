---
title:                "Ruby: Unterschneidungen extrahieren"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum
In vielen Fällen müssen Ruby-Programmiererinnen und Programmierer Strings in kleinere Teile zerlegen. Ein häufiges Beispiel dafür ist die Extraktion von Teilstrings aus einer URL.

## Wie geht das?
In Ruby gibt es eine Methode namens `slice`, die verwendet werden kann, um Teilstrings aus einer Zeichenkette auszuschneiden. Die Methode akzeptiert zwei Parameter: den Startindex und die Länge des Teilstrings.

```Ruby
url = "http://www.example.com/ruby-tutorial"
```
```Ruby
puts url.slice(11, 10)
```
Dieser Code würde den Teilstring "ruby-tutorial" aus dem ursprünglichen String ausschneiden und ausgeben. Beachten Sie, dass der Startindex bei 0 beginnt.

## Tieferer Einblick
Die `slice`-Methode in Ruby ist sehr flexibel und kann auch mit anderen Datentypen wie Arrays und Hashes verwendet werden. Außerdem gibt es noch eine andere Methode namens `substring`, die ebenfalls Teilstrings extrahieren kann.

Beachten Sie jedoch, dass bei `substring` der zweite Parameter die Endposition des Teilstrings anstelle der Länge ist.

## Siehe auch
- Die offizielle Ruby-Dokumentation zur `slice`-Methode: https://ruby-doc.org/core-2.7.2/String.html#method-i-slice
- Ein weiteres Tutorial zur Extraktion von Teilstrings in Ruby: https://www.rubyguides.com/2019/07/ruby-extract-substring/