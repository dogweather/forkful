---
title:    "Elixir: Das aktuelle Datum erhalten"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Elixir ist eine angesagte Programmiersprache, die immer mehr Aufmerksamkeit von Entwicklern auf der ganzen Welt gewinnt. Möglicherweise fragen Sie sich, warum Sie sich die Mühe machen sollten, das aktuelle Datum in Elixir zu erhalten. Die Wahrheit ist, dass es in vielen Anwendungsfällen sehr nützlich sein kann, das Datum als Teil Ihrer Programmlogik zu haben. Sei es für die Aufzeichnung von Zeitstempeln, die Berechnung von Altersangaben oder einfach nur um Ihr Programm dynamischer zu gestalten, der Zugriff auf das aktuelle Datum ist eine wichtige Funktion in jeder Programmiersprache.

## Wie man das aktuelle Datum in Elixir erhält

Das Abrufen des aktuellen Datums in Elixir ist sehr einfach und unkompliziert. Alles, was Sie tun müssen, ist die `Date.utc_today` Funktion aufzurufen und das zurückgegebene Datum entsprechend zu verarbeiten. Hier ist ein Beispiel:

```Elixir
def get_current_date() do
  today = Date.utc_today()
  IO.puts("Das aktuelle Datum ist #{today}")
end
```

Dieser Code ruft die `get_current_date` Funktion auf und gibt das aktuelle Datum aus. Wenn Sie diesen Code in Ihrer Elixir-Konsole ausführen, sehen Sie etwas wie dieses als Output:

```
Das aktuelle Datum ist 2020-11-11
```

Natürlich können Sie die Ausgabe auch anpassen, je nachdem, wie Sie das Datum verwenden möchten. Die `Date.utc_today` Funktion unterstützt auch optionale Argumente wie den Zeitbereich (`:gregorian` oder `:julian`) und die Zeitzone (`Date.utc_now()`), die spezifiziert werden können, um die Ergebnisse weiter anzupassen.

## Tiefergehende Informationen zum aktuellen Datum

Wenn Sie mehr über die genauen Details des aktuellen Datums in Elixir erfahren möchten, gibt es einige interessante Informationen, die Sie sich anschauen können. Zum Beispiel verwendet Elixir das Erlang Kalendermodul, um das Datum zu berechnen, wodurch verschiedene Funktionen wie `Date.diff` und `Date.add` verfügbar werden, um das Datum in verschiedene Richtungen zu manipulieren.

Darüber hinaus kann das Elixir-Datumsformat auch mit dem ISO-8601-Standard kompatibel sein, der in vielen anderen Programmen und Sprachen verwendet wird. Dies ist besonders hilfreich, wenn Sie mit verschiedenen Datumsformaten arbeiten und sicherstellen möchten, dass sie kompatibel sind.

## Siehe auch

- [Elixir School: Dates and Times](https://elixirschool.com/de/lessons/advanced/dates)
- [Elixir Documentation: Date](https://hexdocs.pm/elixir/Date.html)
- [Elixir Forum: Getting current date in Elixir](https://elixirforum.com/t/getting-current-date-in-elixir/4966)