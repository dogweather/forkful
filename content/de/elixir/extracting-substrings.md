---
title:                "Unterstrings extrahieren"
html_title:           "Elixir: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Entnehmen von Teilstrings bezieht sich auf die Aktion, bestimmte Teile eines Strings aus einem größeren Text zu extrahieren. Programmierer verwenden dies, um einen bestimmten Teil eines Strings zu isolieren, der für die Ausführung ihres Codes benötigt wird.

## Wie tue ich das?

Hier sind einige Beispiele zur Verwendung von ```Elixir String.split```:

```
iex> String.split("Hallo Welt", " ") 
["Hallo", "Welt"]

iex> String.split("123-456-789", "-") 
["123", "456", "789"]

iex> String.split("Das ist ein längerer Satz", " ", trim: true) 
["Das", "ist", "ein", "längerer", "Satz"]

iex> String.split("Hund, Katze, Maus", [", ", ","]) 
["Hund", "Katze", "Maus"]

```

## Tiefergehende Analyse

Das Entnehmen von Teilstrings ist eine häufig verwendete Methode in der Programmierung, um eine bestimmte Anzahl von Zeichen aus einem längeren String auszuwählen. Beispielsweise kann dies für die Verarbeitung von Nutzereingaben oder für das Durchsuchen großer Textdateien nützlich sein.

Alternativ zum Entnehmen von Teilstrings kann auch das Öffnen und Lesen von Dateien verwendet werden, um bestimmte Informationen zu extrahieren. In Elixir kann dies mit der Funktion "File.open" durchgeführt werden.

Es gibt auch mehrere Implementierungen des Entnehmens von Teilstrings in Elixir, z.B. die Funktionen "String.slice" und "String.replace".

## Siehe auch

- [Elixir String.split-Dokumentation](https://hexdocs.pm/elixir/String.html#split/2)
- [Elixir File.open-Dokumentation](https://hexdocs.pm/elixir/File.html#open/2)
- [Elixir String.slice-Dokumentation](https://hexdocs.pm/elixir/String.html#slice/3)
- [Elixir String.replace-Dokumentation](https://hexdocs.pm/elixir/String.html#replace/3)