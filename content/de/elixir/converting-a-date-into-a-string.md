---
title:                "Eine Datum in einen String umwandeln"
html_title:           "Elixir: Eine Datum in einen String umwandeln"
simple_title:         "Eine Datum in einen String umwandeln"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Warum?

Wenn du schon einmal mit Datumsangaben in deiner Elixir-Anwendung gearbeitet hast, dann weißt du, dass es manchmal notwendig ist, sie in einen String zu konvertieren. Dies kann nützlich sein, um das Datum in einem menschenlesbaren Format anzuzeigen oder es für die weitere Verarbeitung in einer Datenbank zu speichern.

# Wie geht das?

Die Konvertierung von einem Datum in einen String in Elixir ist ganz einfach. Schauen wir uns dazu ein Beispiel an:

```elixir
datum = ~D[2021-08-10]
String.to_charlist(datum, "dd.MM.yyyy")
```

Das Ergebnis dieses Codes wäre der String "10.08.2021". Zuerst wird das Datum in einen String umgewandelt und dann das gewünschte Format angegeben. Es ist auch möglich, zusätzliche Parameter hinzuzufügen, wie zum Beispiel die Formatierung von Stunden oder Minuten.

Es ist wichtig zu beachten, dass die Funktion `String.to_charlist` nur mit Date-Objekten funktioniert, die mit dem `~D`-Präfix erstellt wurden. Wenn du ein Datum aus anderen Datentypen erstellen möchtest, kannst du die Funktion `:calendar.date_to_gregorian_days/2` verwenden.

## Deep Dive

Die Funktion `String.to_charlist` verwendet intern die `:calendar.strftime/2` Funktion, die Teile einer Date im gewünschten Format ausgibt. Wenn wir also das Format `"dd.MM.yyyy"` angeben, bedeutet das, dass die Funktion den Tag, den Monat und das Jahr in der richtigen Reihenfolge ausgibt. 

Es ist auch möglich, eigene benutzerdefinierte Formatierungen zu erstellen, indem du das `~`-Zeichen vor dem Formatierungscode setzt und diese innerhalb von `{}` Klammern platzierst. Zum Beispiel:

```elixir
String.to_charlist(datum, "~{~a.~} ~B ~Y", chars: [".", ""])
```

Dies würde das Datum im Format "10. August 2021" ausgeben.

# Siehe auch

- [Elixir Dokumentation für Kalenderfunktionen](https://hexdocs.pm/elixir/Calendar.html)
- [Einführung in Elixir Dates und Times](https://elixirschool.com/en/lessons/advanced/datetime/)
- [Elixir Date and Time Library](https://github.com/bitwalker/timex)