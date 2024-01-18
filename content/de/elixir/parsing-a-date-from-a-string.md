---
title:                "Das Parsen eines Datums aus einem String"
html_title:           "Elixir: Das Parsen eines Datums aus einem String"
simple_title:         "Das Parsen eines Datums aus einem String"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Was & Warum?
Das Parsen eines Datums aus einer Zeichenfolge ist ein häufiges Problem bei der Entwicklung von Software. Dabei geht es darum, ein Datum aus einer Zeichenfolge zu extrahieren und in ein Datumobjekt umzuwandeln. Programmierer nutzen diese Funktion, um Nutzereingaben oder Daten aus externen Quellen zu verarbeiten.

Wie geht's?
In Elixir gibt es mehrere Möglichkeiten, ein Datum aus einer Zeichenfolge zu parsen. Eine der einfachsten Methoden ist die Verwendung der Funktion ```Date.from_iso8601/1```, die eine ISO 8601-Formatierte Zeichenfolge akzeptiert und ein Datumobjekt zurückgibt. Hier ist ein Beispiel:

```Elixir
iex> Date.from_iso8601("2020-06-10")
{:ok, ~D[2020-06-10]}
```

Es ist auch möglich, benutzerdefinierte Formate zu verwenden, indem man die Funktion ```Date.from_string/2``` verwendet und das gewünschte Format als zweiten Parameter angibt. Hier ist ein Beispiel mit dem Format "dd/mm/yyyy":

```Elixir
iex> Date.from_string("10/06/2020", "dd/mm/yyyy")
{:ok, ~D[2020-06-10]}
```

Tiefer schürfen
Das Parsen von Datumsangaben aus Zeichenfolgen ist ein Problem, das schon seit langem existiert und von Programmierern auf der ganzen Welt gelöst wurde. In anderen Sprachen gibt es ähnliche Funktionen, z.B. ```datetime.strptime``` in Python oder ```DateTime.parse``` in Ruby. Alternativ kann man auch Bibliotheken wie das Elixir-Paket ```timex``` verwenden, um erweiterte Funktionen für die Datummanipulation zur Verfügung zu haben.

Nicht zu vergessen ist, dass das Parsen von Datumsangaben nicht nur in Programmen vorkommt, sondern auch in alltäglichen Situationen. Denken Sie z.B. an die Umwandlung von geschriebenen Datumangaben in ein einheitliches Format wie den ISO 8601. Das parsen von Datumsangaben ist also nicht nur ein technisches Problem, sondern auch ein allgemeines Problem.

Schau auch mal hier:
- [Elixir-Dokumentation zu Date](https://hexdocs.pm/elixir/Date.html)
- [Elixir-Dokumentation zu timex](https://hexdocs.pm/timex/Timex.html)
- [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)