---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist ein häufig in der Programmierung verwendeter Ansatz zum Bereinigen oder Manipulieren von Daten-String. Es ist nützlich, um unerwünschte Zeichen aus Textdaten zu entfernen, und ermöglicht eine akkurate Datenanalyse und -verarbeitung.

## Wie man es macht:
Um Zeichen entsprechend einem Muster in Elixir zu löschen, nutzen wir die `Regex.replace/3` Funktion. Hier ist ein einfaches Beispiel:

```Elixir
text = "Dies ist ein Beispiel. \n Ein ziemlich einfaches Beispiel."
neuer_text = Regex.replace(~r/\s+/, text, "")
IO.puts neuer_text
```
Die Ausgabe wäre:
```Elixir
"DiesisteinBeispiel.EinziemlicheinfachesBeispiel."
```
Das `~r/\s+/` ist das Muster, das alle Arten von Leerzeichen (einschließlich Zeilenumbrüche und Tabs) innerhalb des Strings erfasst und entfernt.

## Tiefere Einblicke
Historisch gesehen war das durch Muster übereinstimmende Löschen von Zeichen schon immer ein wichtiger Bestandteil von Textverarbeitungs- und Programmiersprachen, eine Funktion, die Traditionen aus Perl und Java aufgreift. 

In Elixir erfolgt die Implementierung dieses Feature durch den `:re` Erlang Modul, welcher auf dem PCRE (Perl Compatible Regular Expressions) aufbaut.

Alternativ dazu kann man auch die `String.replace/3` Funktion für einfache Zeichenersetzungen verwenden, aber `Regex.replace/3` bietet durch seine Nutzung von Regulären Ausdrücken eine weitaus größere Flexibilität und Kontrolle.

## Siehe auch
Erlang `:re` Modul Dokumentation: http://erlang.org/doc/man/re.html
Elixir `Regex` Modul Dokumentation: https://hexdocs.pm/elixir/Regex.html
Elixir `String` Modul Dokumentation: https://hexdocs.pm/elixir/String.html
PCRE Dokumentation: https://www.pcre.org/original/doc/html/index.html