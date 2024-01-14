---
title:                "Elixir: Vergleich von zwei Datumangaben"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum 

Vergleichen von zwei Daten in Elixir ist eine nützliche Fähigkeit, die jeder Programmierer beherrschen sollte. Es erlaubt uns, komplexe logische Bedingungen zu erstellen und zu überprüfen, ob ein Ereignis in der Zukunft oder Vergangenheit liegt. Lesen Sie weiter, um zu erfahren, wie Sie dies in Elixir tun können.

## Wie geht das?

Um zwei Daten in Elixir zu vergleichen, können wir die `DateTime.compare/2` Funktion verwenden. Diese Funktion nimmt zwei `DateTime` Objekte als Argumente und gibt einen von drei Atomwerten zurück: `:gt` wenn das erste Datum größer als das zweite ist; `:eq` wenn beide Daten gleich sind; und `:lt` wenn das erste Datum kleiner als das zweite ist.

Nehmen wir zum Beispiel die folgenden Daten an:

```Elixir
first_date = ~N[2020-01-01 13:00:00]
second_date = ~N[2020-01-02 12:00:00]
```

Mit der `DateTime.compare/2` Funktion können wir nun überprüfen, welches der beiden Daten größer ist:

```Elixir
DateTime.compare(first_date, second_date)
# => :lt
```

Wir können diese Funktion auch verwenden, um zu überprüfen, ob ein Ereignis in der Zukunft oder Vergangenheit liegt, indem wir ein Datum als Argument und `DateTime.utc_now` als zweites Argument übergeben:

```Elixir
DateTime.compare(~N[2020-03-01], DateTime.utc_now)
# => :gt
```

Das heißt, das Ereignis liegt in der Zukunft, da das erste Datum größer als das aktuelle Datum ist.

## Eintauchen

Wenn wir genauer auf die `DateTime.compare/2` Funktion schauen, werden wir feststellen, dass sie auch auf der `DateTime.compare/4` Funktion basiert. Diese Funktion erlaubt es uns, ein Datum in verschiedenen Zeitzonen zu vergleichen.

Eine weitere wichtige Funktion beim Vergleichen von Daten ist die `DateTime.diff/2` Funktion. Diese gibt die Differenz zwischen zwei Daten in Sekunden zurück. Dies kann nützlich sein, um zu überprüfen, wie lange ein Ereignis in der Vergangenheit oder Zukunft liegt.

Es ist auch wichtig zu beachten, dass beide Funktionen die Ordnung von Datum und Uhrzeit berücksichtigen. Das heißt, dass ein Datum vor einer Uhrzeit steht.

## Siehe auch

- [Elixir Dokumentation über DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Einführung in die Datums- und Zeitfunktionen in Elixir](https://medium.com/@ellie_koola/introduction-to-date-and-time-functions-in-elixir-1bd7c961c69e)
- [Die Macht der Mustererkennung in Elixir](https://elixir-lang.org/blog/2018/08/07/elixir-and-pattern-matching/)