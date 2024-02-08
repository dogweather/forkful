---
title:                "Einen String großschreiben"
aliases:
- de/elixir/capitalizing-a-string.md
date:                  2024-02-03T19:04:49.285021-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen String großschreiben"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Kapitalisieren eines Strings bedeutet, den ersten Buchstaben des Strings in Großbuchstaben umzuwandeln, während sichergestellt wird, dass alle anderen Buchstaben in Kleinbuchstaben sind. Diese Aktion ist häufig notwendig, um Benutzereingaben zu formatieren oder Text in Benutzeroberflächen anzuzeigen, wo Konsistenz und Lesbarkeit wichtig sind.

## Wie geht das:

Elixir bietet eine unkomplizierte Möglichkeit, Strings zu kapitalisieren, indem es seine eingebauten Funktionen nutzt, ohne dass Drittanbieter-Bibliotheken erforderlich sind. Hier ist ein einfaches Beispiel:

```elixir
string = "elixir programmierung"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

Ausgabe:

```
Elixir programmierung
```

Für Fälle, in denen mehr Kontrolle oder eine komplexere Kapitalisierungslogik benötigt wird, könnten Sie verschiedene String-Funktionen kombinieren. Wenn Sie beispielsweise jedes Wort in einem Satz großschreiben möchten, können Sie den Satz in Wörter teilen, jedes kapitalisieren und dann wieder zusammenfügen:

```elixir
sentence = "elixir macht spaß"
capitalized_sentence = sentence 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts capitalized_sentence
```

Ausgabe:

```
Elixir Macht Spaß
```

Obwohl die Standardbibliothek von Elixir die meisten Bedürfnisse abdeckt, könnten Sie für eine nuanciertere Textmanipulation, einschließlich fortgeschrittener String-Kapitalisierung, Drittanbieter-Bibliotheken wie Cldr für Internationalisierung erforschen, die verhaltensspezifische Kapitalisierung nach Lokalität anbieten können.
