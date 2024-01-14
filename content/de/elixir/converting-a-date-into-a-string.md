---
title:    "Elixir: Umwandlung eines Datums in einen String"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Warum
Im Elixir Programmieren gibt es oft die Notwendigkeit, ein Datum in einen String umzuwandeln. Dies kann für verschiedene Zwecke wie Datenbankabfragen, Berechnungen oder die Darstellung von Daten in einem bestimmten Format erforderlich sein.

## Wie geht das?
In Elixir gibt es verschiedene Methoden, um ein Datum in einen String umzuwandeln. Die einfachste Möglichkeit ist die Verwendung der Funktion `to_string`. Hier ein Beispielcode:

```Elixir
date = ~D[2021-01-01]
string = to_string(date)
IO.puts string
```

Die Ausgabe dieses Codes wird `2021-01-01` sein. Wie Sie sehen können, hat die Funktion `to_string` das Datum in einen String im Format `YYYY-MM-DD` umgewandelt.

Eine weitere Möglichkeit ist die Verwendung der Funktion `DateTime.to_iso8601`. Diese Funktion gibt das Datum in ISO-8601-Format zurück, das viele APIs und Datenbanken verwenden. Hier ein Beispielcode:

```Elixir
date = ~D[2021-01-01]
string = DateTime.to_iso8601(date)
IO.puts string
```

Die Ausgabe dieses Codes wird `2021-01-01T00:00:00Z` sein. Beachten Sie, dass die Funktion `DateTime.to_iso8601` auch die Zeit und die Zeitzone des Datums berücksichtigt.

## Tiefergehende Informationen
Beim Konvertieren eines Datums in einen String ist es wichtig zu verstehen, dass das Datum im Elixir immer als Elixir-Struktur gespeichert wird. Dies bietet Flexibilität und ermöglicht es uns, verschiedene Funktionen je nach Anforderung zu verwenden.

Darüber hinaus gibt es in Elixir auch die Möglichkeit, benutzerdefinierte Formate für Datum und Uhrzeit zu erstellen. Dafür gibt es die Funktion `Date.to_string`. Hier ein Beispielcode:

```Elixir
date = ~D[2021-01-01]
string = Date.to_string(date, "{0}/{1}/{2}")
IO.puts string
```

Die Ausgabe dieses Codes wird `01/01/2021` sein, da wir ein benutzerdefiniertes Format im Format `MM/DD/YYYY` angegeben haben.

## Siehe auch
- [Date.to_string Dokumentation](https://hexdocs.pm/elixir/Date.html#to_string/2)
- [DateTime.to_iso8601 Dokumentation](https://hexdocs.pm/elixir/DateTime.html#to_iso8601/2)
- [Date.to_string benutzerdefiniertes Format Dokumentation](https://hexdocs.pm/elixir/Date.html#to_string/1)

Ich hoffe, dieser Beitrag hat Ihnen geholfen, die verschiedenen Möglichkeiten zum Konvertieren eines Datums in einen String in Elixir zu verstehen. Viel Spaß beim Programmieren!