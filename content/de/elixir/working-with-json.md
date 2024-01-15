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

## Warum

 Willst du deine Programmierkenntnisse verbessern und in einer modernen Sprache wie Elixir arbeiten? Dann ist es wichtig, dass du auch mit JSON umgehen können. JSON ist ein Datenaustauschformat, das in vielen Anwendungen und APIs verwendet wird. Elixir bietet eine einfache und leistungsstarke Möglichkeit, mit JSON umzugehen, und in diesem Artikel werde ich dir zeigen, wie du das machen kannst.

## Wie geht das

Um mit JSON in Elixir zu arbeiten, musst du zuerst das `Jason` Paket in deinem Projekt einbinden. Dies kannst du einfach mit dem folgenden Befehl in deiner Konsole machen: 
```
mix deps.get jason
```

Als nächstes musst du das Paket in deinem Modul importieren:
```
defmodule MeinModul do
  use Jason
  
  # Hier kannst du deine Funktionen schreiben
end
```

Sobald das Paket eingebunden ist, kannst du mit dem `Jason.encode!/1` und `Jason.decode!/1` Funktionen JSON-Daten in Elixir umwandeln. Hier ist ein Beispiel für die Verwendung von `encode!/1`:
```
iex> Jason.encode!(%{"name" => "Max", "age" => 26})
"{\"name\":\"Max\",\"age\":26}"
```

Und hier ist ein Beispiel für `decode!/1`:
```
iex> Jason.decode!("{\"name\":\"Max\",\"age\":26}")
%{"name" => "Max", "age" => 26}
```

Wie du siehst, ist die Verwendung von JSON in Elixir sehr einfach und intuitiv.

## Tiefgründiger Einblick

Die `Jason` Bibliothek bietet auch viele weitere Funktionen, um mit JSON zu arbeiten. Dazu gehört unter anderem die Möglichkeit, Maßgeschneiderte Encoder und Decoder zu erstellen, die spezifische Datenstrukturen in JSON übersetzen können. Außerdem bietet das Paket auch die Möglichkeit, JSON-Daten direkt in Elixir-Datenstrukturen umzuwandeln, ohne dass sie als Strings behandelt werden müssen.

Es gibt auch andere Elixir-Pakete, die mit JSON arbeiten, wie z.B. `Poison` und `Jazz` - es lohnt sich, sie anzuschauen und zu sehen, was am besten für deine Bedürfnisse geeignet ist.

## Siehe auch

- [Elixir Dokumentation](https://elixir-lang.org/getting-started/introduction.html)
- [Jason GitHub Repository](https://github.com/michalmuskala/jason)
- [Poison GitHub Repository](https://github.com/devinus/poison)