---
title:                "Elixir: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Strings zu Kleinbuchstaben kann hilfreich sein, wenn man beispielsweise nach Zeichenfolgen sucht oder sie vergleichen möchte. Es ist eine gängige Aufgabe in der Programmierung von Alltagssprachen wie Elixir.

## Wie geht es

Das Umwandeln von Strings zu Kleinbuchstaben in Elixir ist einfach. Hier ist ein kurzer Code-Beispiel, um den Vorgang zu demonstrieren:

```Elixir
str = "GUTEN MORGEN" 
IO.puts(String.downcase(str))
```
Dieser Code gibt "guten morgen" auf der Konsole aus. Wie du sehen kannst, ist der Vorgang sehr einfach und erfordert keine zusätzlichen Bibliotheken oder Funktionen. Elixir bietet eine eingebaute Funktion, um Strings in Kleinbuchstaben zu konvertieren.

## Tiefer eintauchen

Obwohl die Konvertierung von Strings zu Kleinbuchstaben einfach erscheint, gibt es einige Dinge, die man beachten sollte. Zum Beispiel berücksichtigt Elixir bei der Konvertierung die verschiedenen Schriftarten und -zeichen, die in einer Sprache verwendet werden können. Hier sind einige weitere Beispiele für den Umgang mit Sonderfällen:

```Elixir
IO.puts(String.downcase("Straße"))
IO.puts(String.downcase("İstanbul"))
```

Das erste Beispiel gibt "straße" und das zweite Beispiel gibt "i̇stanbul" aus. Wie du siehst, werden auch akzentuierte Buchstaben oder spezielle Schriftzeichen richtig konvertiert. Damit zeigt Elixir, dass es sich um eine robuste Sprache handelt, die an die Bedürfnisse von Benutzern angepasst werden kann.

Nun kannst du dich zurücklehnen und deine Elixir-Anwendungen entwickeln, ohne dir Gedanken über die Konvertierung von Strings zu Kleinbuchstaben machen zu müssen.

## Siehe auch
- Offizielle Elixir-Dokumentation zu String-Funktionen (https://hexdocs.pm/elixir/String.html)
- Ebenfalls auf Deutsch: Elixir-Tutorial-Serie von Elixir Blog (https://elixir-blog.de/tutorial-erste-schritte-mit-elixir)