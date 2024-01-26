---
title:                "Anführungszeichen aus einem String entfernen"
date:                  2024-01-26T03:39:06.450740-07:00
model:                 gpt-4-0125-preview
simple_title:         "Anführungszeichen aus einem String entfernen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Anführungszeichen aus einem String zu entfernen bedeutet, diese zusätzlichen Verpackungen abzulegen, um den sauberen Text im Inneren zu erhalten. Programmierer tun dies, um Eingaben zu bereinigen, Fehler zu vermeiden und Daten für die Verarbeitung vorzubereiten, bei der Anführungszeichen eher Störfaktoren als Funktionen sind.

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