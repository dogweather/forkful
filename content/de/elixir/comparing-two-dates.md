---
title:    "Elixir: Zwei Daten vergleichen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Datumvergleiche sind eine wichtige Funktion in jeder Programmiersprache, einschließlich Elixir. Sie ermöglichen es uns, schnell und einfach zu überprüfen, ob ein Datum früher, später oder gleich einem anderen Datum ist. In diesem Blog-Beitrag werden wir uns ansehen, wie man zwei Daten in Elixir vergleicht und tiefer in diese Funktionalität eintauchen.

## Wie geht es?

``` Elixir
  # Vergleich von zwei Datumsangaben
  date1 = ~D[2020-06-01]
  date2 = ~D[2020-06-05]
  date3 = ~D[2020-06-03]
  
  # Überprüfen, ob date2 später als date1 ist
  date2 > date1 # Ausgabe: true
  
  # Überprüfen, ob date3 vor oder gleich date1 ist
  date3 <= date1 # Ausgabe: true
  
  # Überprüfen, ob date2 und date3 gleich sind
  date2 == date3 # Ausgabe: false
```
Wie man sehen kann, gibt es in Elixir Operatoren, die speziell für Datumsvergleiche verwendet werden können. Der `>` Operator gibt `true` zurück, wenn der linke Wert später als der rechte Wert ist. Der `<=` Operator gibt `true` zurück, wenn der linke Wert vor oder gleich dem rechten Wert ist. Und der `==` Operator gibt `true` zurück, wenn beide Werte gleich sind.

## Tiefentauchen

Während wir bereits einige grundlegende Vergleiche gesehen haben, können wir auch tiefer in die Funktionalität von Elixirs `Date` Modul eintauchen und mehr Möglichkeiten zum Vergleichen von Daten entdecken. Zum Beispiel können wir das `Date.compare/2` Funktion verwenden, um zu überprüfen, welches Datum früher oder später als das andere ist.

``` Elixir
  # Vergleich von zwei Datumsangaben
  date1 = ~D[2020-06-01]
  date2 = ~D[2020-06-05]
  date3 = ~D[2020-06-03]
  
  # Überprüfen, ob date2 später als date1 ist
  Date.compare(date2, date1) # Ausgabe: :gt
  
  # Überprüfen, ob date3 vor oder gleich date1 ist
  Date.compare(date3, date1) # Ausgabe: :lt
  
  # Überprüfen, ob date2 und date3 gleich sind
  Date.compare(date2, date3) # Ausgabe: :eq
```
Wie man sieht, gibt die `Date.compare/2` Funktion einen von drei Atom-Attributen zurück: `:gt` für größer, `:lt` für kleiner und `:eq` für gleich. Dies kann besonders nützlich sein, wenn man die Vergleiche in anderen Funktionen oder Verzweigungen verwendet.

## Siehe Auch

- [Elixir Date Modul Dokumentation](https://hexdocs.pm/elixir/Date.html)
- [Elixir Datumsvergleich Tutorial](https://elixirschool.com/de/lessons/specifics/comparing-dates/) 
- [Elixir Vergleichsoperatoren](https://elixirschool.com/de/lessons/basics/operators/)