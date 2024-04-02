---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:00.549342-07:00
description: "Citaten uit een string verwijderen betekent die extra verpakkingen wegnemen\
  \ om de schone tekst binnenin te krijgen. Programmeurs doen dit om invoer te\u2026"
lastmod: '2024-03-13T22:44:50.450072-06:00'
model: gpt-4-0125-preview
summary: "Citaten uit een string verwijderen betekent die extra verpakkingen wegnemen\
  \ om de schone tekst binnenin te krijgen. Programmeurs doen dit om invoer te\u2026"
title: Quotes verwijderen uit een string
weight: 9
---

## Wat & Waarom?
Citaten uit een string verwijderen betekent die extra verpakkingen wegnemen om de schone tekst binnenin te krijgen. Programmeurs doen dit om invoer te zuiveren, fouten te vermijden en data voor te bereiden voor verwerking waar citaten hinderlijk zijn, geen functies.

## Hoe:
Elixir heeft geen ingebouwde 'verwijder citaten'-functie, maar het is kinderspel om er zelf een te maken met patroonmatching of `String`-functies. Zie deze snippets:

```elixir
# Gebruikmakend van patroonmatching
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# Voorbeeldgebruik
unquote_string("\"Hallo, Wereld!\"") # => "Hallo, Wereld!"
unquote_string("'Hallo, Wereld!'")   # => "Hallo, Wereld!"

# Gebruikmakend van String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# Voorbeeldgebruik
unquote_string("\"Hallo, Wereld!\"") # => "Hallo, Wereld!"
unquote_string("'Hallo, Wereld!'")   # => "Hallo, Wereld!"
```

Uitvoer voor beide methodes zal zijn:
```
"Hallo, Wereld!"
```

## Diepere Duik
In het verleden waren citaten in strings een mijnenveld - behandel ze verkeerd, en boem, syntaxisfouten of beveiligingslekken. In Elixir behandelt patroonmatching je strings als Lego-blokken, waardoor je ze nauwkeurig kunt uit elkaar halen en weer opbouwen. De robuuste `String`-module komt ook van pas, die flexibel citaten verwijdert met `trim`-functies. De alternatieven? Reguliere expressies kunnen citaten naar de curb schoppen, en externe bibliotheken kunnen extra vuurkracht bieden als je meer nodig hebt dan alleen basisstrippen.

## Zie Ook
Duik dieper in deze onderwerpen:
- [Elixir's String module](https://hexdocs.pm/elixir/String.html)
- [Leer meer over patroonmatching in Elixir](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Reguliere expressies in Elixir (Regex module)](https://hexdocs.pm/elixir/Regex.html)
