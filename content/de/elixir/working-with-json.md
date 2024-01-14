---
title:                "Elixir: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Warum
In der Welt der Programmierung gibt es viele Datenformate, mit denen Entwicklerinnen und Entwickler umgehen müssen. Eines dieser Formate ist JSON, das in der heutigen Zeit immer beliebter wird. Es ist ein leicht lesbares und einfaches Format, das häufig für den Austausch von Daten zwischen verschiedenen Anwendungen verwendet wird. Aber warum sollte man sich mit JSON beschäftigen? Nun, es kann eine äußerst nützliche Fähigkeit sein, die bei der Arbeit mit APIs oder dem Parsen von Dateien hilft. In diesem Blog-Beitrag werden wir uns damit beschäftigen, wie wir mit JSON in Elixir arbeiten können.

## Wie man mit JSON in Elixir arbeitet
Um mit JSON in Elixir arbeiten zu können, müssen wir das Modul `Poison` importieren, das eine Bibliothek für die Verarbeitung von JSON ist. Wir können es mit dem Befehl `mix deps.get` in unsere Elixir-Anwendung einbinden. Dann können wir `Poison` wie folgt verwenden:

```Elixir
# Erstellen eines JSON-Strings
json = %{"name" => "Max", "age" => 28}

# Konvertieren des JSON-Strings in ein Elixir-Mapping
elixir_mapping = Poison.encode(json)
# %{name: "Max", age: 28}

# Konvertieren eines Elixir-Mappings in einen JSON-String
encoded_json = Poison.decode(elixir_mapping)
# %{"name" => "Max", "age" => 28}
```

Wie wir sehen können, verwendet `Poison` die Funktionen `encode/1` und `decode/1`, um zwischen JSON-Strings und Elixir-Mappings zu konvertieren. Wir können auch zusätzliche Optionen für die Formatierung des JSON-Strings angeben, z.B. indem wir `pretty: true` als Argument in `Poison.encode/2` übergeben.

## Tiefentauchen
Erfahrene Elixir-Entwicklerinnen und -Entwickler wissen vielleicht bereits, dass es noch eine andere Bibliothek für die Verarbeitung von JSON gibt - `Jason`. Im Gegensatz zu `Poison` ist `Jason` in reinem Elixir geschrieben und bietet einige zusätzliche Funktionen. Wenn man sehr viel mit JSON in einer Elixir-Anwendung arbeitet, kann es sich lohnen, auch `Jason` zu erkunden und zu entscheiden, welche Bibliothek für welche Anforderungen am besten geeignet ist.

## Siehe auch
- [Offizielle Elixir Dokumentation für JSON](https://hexdocs.pm/poison/readme.html)
- [JSON in der Elixir Schule](https://elixirschool.com/de/lessons/specifics/json/)
- [GitHub-Repository von Jason](https://github.com/michalmuskala/jason)