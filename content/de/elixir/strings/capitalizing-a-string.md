---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:49.285021-07:00
description: "Das Kapitalisieren eines Strings bedeutet, den ersten Buchstaben des\
  \ Strings in Gro\xDFbuchstaben umzuwandeln, w\xE4hrend sichergestellt wird, dass\
  \ alle anderen\u2026"
lastmod: '2024-03-13T22:44:53.445813-06:00'
model: gpt-4-0125-preview
summary: "Das Kapitalisieren eines Strings bedeutet, den ersten Buchstaben des Strings\
  \ in Gro\xDFbuchstaben umzuwandeln, w\xE4hrend sichergestellt wird, dass alle anderen\
  \ Buchstaben in Kleinbuchstaben sind."
title: "Einen String gro\xDFschreiben"
weight: 2
---

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
