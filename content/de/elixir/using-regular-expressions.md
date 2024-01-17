---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Elixir: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Was & Warum?
Verwenden von regulären Ausdrücken ist eine Möglichkeit, Zeichenfolgen in einem Text zu suchen und zu manipulieren. Programmierer nutzen dies, um spezifische Patterns in großen Datenmengen zu finden oder Eingaben von Benutzern zu validieren.

# Wie geht's?
Elixir hat ein integriertes Modul namens Regex, das es dir ermöglicht, reguläre Ausdrücke zu erstellen und auf Strings anzuwenden.

```Elixir 
# Beispiele

# Überprüfe, ob eine E-Mail-Adresse gültig ist
Regex.match?("^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,4}$", "example123@email.com")
# => true

# Finde alle Wörter, die mit "Elixir" beginnen
Regex.scan(~r/Elixir\w*/, "Elixir ist eine Funktionale Programmiersprache")
# => ["Elixir", "Elixir ist"]

# Ersetze alle Vokale durch Großbuchstaben
Regex.replace(~r/[aeiou]/, "Hallo", &String.upcase/1)
# => "HAllO"
```

# Tiefere Einblicke
Reguläre Ausdrücke gibt es schon seit den 1940er Jahren und wurden zunächst von Mathematikern entwickelt. Es gibt auch andere Möglichkeiten, Zeichenfolgen zu durchsuchen und zu manipulieren, wie z.B. die Verwendung von String-Methoden, aber reguläre Ausdrücke sind oft effizienter und leistungsfähiger. Das Elixir-Modul Regex basiert auf der PCRE-Bibliothek, was bedeutet, dass es einige Besonderheiten bei der Syntax gibt, die es zu beachten gilt.

# Siehe auch
- Offizielle Dokumentation zu regulären Ausdrücken in Elixir: https://hexdocs.pm/elixir/Regex.html
- Ein nützliches Cheat Sheet mit regulären Ausdrücken für Elixir: https://devhints.io/elixir-regex
- Ein Elixir-Blog-Beitrag über die Verwendung von regulären Ausdrücken für Textvalidierung: https://blog.codeship.com/elixir-playing-with-regular-expressions/