---
title:                "Arbeiten mit JSON"
html_title:           "Elixir: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/working-with-json.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Arbeiten mit JSON ist ein wichtiger Bestandteil der modernen Programmierung. JSON steht für JavaScript Object Notation und ist ein Format zum Austausch von Daten zwischen Servern und Anwendungen. Programmierer nutzen JSON, weil es einfach zu lesen und zu schreiben ist und da es ein beliebtes Format für API-Endpunkte ist.

# Wie geht das?
Elixir hat eine integrierte Bibliothek namens `Jason`, die die Verarbeitung von JSON-Daten ermöglicht. Zum Beispiel können wir Daten in ein JSON-Objekt umwandeln:
```Elixir
data = %{name: "Max", age: 25}
json = Jason.encode!(data)
```
Dieser Code nimmt ein beliebiges Elixir-Map-Objekt und wandelt es in das äquivalente JSON um.

Und umgekehrt können wir JSON in ein Elixir-Objekt umwandeln:
```Elixir
json = "{\"name\": \"Max\", \"age\": 25}"
data = Jason.decode!(json)
```
Dieser Code liest einen JSON-String und wandelt ihn in ein Elixir-Map-Objekt um.

# Tief eintauchen
JSON wurde in den frühen 2000er Jahren als Alternative zum damals populären XML-Format entwickelt. Es ist leichtgewichtiger und einfacher zu lesen als XML, was es zu einer beliebten Wahl für den Datenaustausch gemacht hat.

Obwohl Elixir die `Jason`-Bibliothek als Standard hat, gibt es auch andere Bibliotheken, wie z.B. `Poison` oder `Jiffy`, die ähnliche Funktionen haben. Es lohnt sich, sie zu vergleichen und diejenige auszuwählen, die am besten zu Ihrem Projekt passt.

Die `Jason`-Bibliothek ist in Elixir eingebaut und erfordert keine zusätzlichen Installationen. Außerdem unterstützt sie sowohl die Verarbeitung von JSON im Speicher als auch direkt von einer URL. Sie kann auch für größere Datenmengen effizient verwendet werden.

# Siehe auch
- Dokumentation für [Jason](https://hexdocs.pm/jason/readme.html)
- Elixir-Bibliotheken für JSON: [Poison](https://github.com/devinus/poison), [Jiffy](https://github.com/davisp/jiffy)