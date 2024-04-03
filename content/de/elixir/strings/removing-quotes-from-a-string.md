---
date: 2024-01-26 03:39:06.450740-07:00
description: "Wie geht das: Elixir hat keine eingebaute Funktion zum Entfernen von\
  \ Anf\xFChrungszeichen, aber es ist ein Kinderspiel, Ihre eigene Funktion mit\u2026"
lastmod: '2024-03-13T22:44:53.450789-06:00'
model: gpt-4-0125-preview
summary: "Elixir hat keine eingebaute Funktion zum Entfernen von Anf\xFChrungszeichen,\
  \ aber es ist ein Kinderspiel, Ihre eigene Funktion mit Musterabgleich oder `String`-Funktionen\
  \ zu erstellen."
title: "Anf\xFChrungszeichen aus einem String entfernen"
weight: 9
---

## Wie geht das:
Elixir hat keine eingebaute Funktion zum Entfernen von Anführungszeichen, aber es ist ein Kinderspiel, Ihre eigene Funktion mit Musterabgleich oder `String`-Funktionen zu erstellen. Sehen Sie sich diese Snippets an:

```elixir
# Mit Musterabgleich
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# Beispiel Nutzung
unquote_string("\"Hallo, Welt!\"") # => "Hallo, Welt!"
unquote_string("'Hallo, Welt!'")   # => "Hallo, Welt!"

# Mit String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# Beispiel Nutzung
unquote_string("\"Hallo, Welt!\"") # => "Hallo, Welt!"
unquote_string("'Hallo, Welt!'")   # => "Hallo, Welt!"
```

Die Ausgabe beider Methoden wird sein:
```
"Hallo, Welt!"
```

## Tiefere Einblicke
Früher waren Anführungszeichen in Strings ein Minenfeld – handhabt man sie falsch, und bum, Syntaxfehler oder Sicherheitslücken. In Elixir behandelt der Musterabgleich Ihre Strings wie Legosteine, was es Ihnen ermöglicht, sie präzise auseinanderzunehmen und wieder zusammenzubauen. Sein robustes `String`-Modul ist ebenfalls praktisch, es entfernt flexibel Anführungszeichen mit `trim`-Funktionen. Die Alternativen? Reguläre Ausdrücke können Anführungszeichen problemlos beseitigen, und externe Bibliotheken könnten zusätzliche Feuerkraft bieten, wenn Sie mehr als nur einfaches Entfernen benötigen.

## Siehe auch
Vertiefen Sie Ihr Wissen mit diesen Quellen:
- [Elixirs String-Modul](https://hexdocs.pm/elixir/String.html)
- [Mehr über Musterabgleich in Elixir lernen](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Reguläre Ausdrücke in Elixir (Regex-Modul)](https://hexdocs.pm/elixir/Regex.html)
