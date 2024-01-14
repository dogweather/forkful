---
title:    "Ruby: Vergleich von zwei Daten"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Warum

Wenn du ein(e) Ruby Programmierer(in) bist, hast du vielleicht schon einmal gemerkt, dass es manchmal nötig ist, zwei Daten zu vergleichen. Dies kann hilfreich sein, um zum Beispiel zu überprüfen, ob ein bestimmtes Datum in der Vergangenheit oder Zukunft liegt. In diesem Blog-Beitrag werde ich dir zeigen, wie du in Ruby ganz einfach zwei Daten vergleichen kannst.

## Wie geht es?

Um zwei Daten in Ruby zu vergleichen, können wir die `Date` Klasse verwenden. Diese bietet verschiedene Methoden an, die uns bei der Vergleichung helfen. Schauen wir uns an, wie das genau funktioniert:

```Ruby
require 'date'

date1 = Date.new(2020, 5, 10)
date2 = Date.new(2021, 5, 10)

puts "Ist #{date1} vor #{date2}? #{date1 < date2}"
puts "Ist #{date1} nach #{date2}? #{date1 > date2}"
puts "Sind #{date1} und #{date2} gleich? #{date1 == date2}"
```

Das obige Beispiel zeigt, wie wir die `Date` Klasse importieren und dann zwei verschiedene Daten erstellen können. Anschließend verwenden wir die Vergleichsoperatoren `<`, `>` und `==` um die Daten miteinander zu vergleichen. In diesem Fall geben wir ein Boolean zurück, je nachdem ob die Bedingung wahr oder falsch ist.

Die Ausgabe für dieses Beispiel wäre:

```
Ist 2020-05-10 vor 2021-05-10? true
Ist 2020-05-10 nach 2021-05-10? false
Sind 2020-05-10 und 2021-05-10 gleich? false
```

## Tiefer Einblick

Nun, da wir wissen, wie wir zwei Daten vergleichen können, lassen uns noch einen Schritt weitergehen und uns einige weitere Methoden der `Date` Klasse ansehen.

- `include?`: Überprüft, ob ein bestimmtes Datum in einem bestimmten Bereich liegt. Zum Beispiel: `Date.new(2020, 5, 10).include?(Date.new(2020, 5, 1)) #=> true`
- `between?`: Überprüft, ob ein bestimmtes Datum zwischen zwei anderen Daten liegt. Zum Beispiel: `Date.new(2020, 5, 19).between?(Date.new(2020, 5, 1), Date.new(2020, 5, 31)) #=> true`
- `leap?`: Überprüft, ob ein bestimmtes Jahr ein Schaltjahr ist. Zum Beispiel: `Date.new(2020, 5, 1).leap? #=> true`

Diese und weitere Methoden können dir beim Vergleichen von Daten in Ruby helfen. Es ist zunächst vielleicht etwas verwirrend, aber mit etwas Übung wirst du schnell verstehen, welches die beste Methode für deine Anwendung ist.

## Siehe auch

- [Ruby Datumsklasse](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Vergleichsoperatoren in Ruby](https://www.rubyguides.com/2015/03/ruby-comparison-operators/)

Vielen Dank fürs Lesen und viel Spaß beim Vergleichen von Daten in Ruby!