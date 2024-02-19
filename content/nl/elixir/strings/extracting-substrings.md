---
aliases:
- /nl/elixir/extracting-substrings/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:45.574607-07:00
description: "Het extraheren van substrings betekent het uithalen van specifieke delen\
  \ uit een tekenreeks. Programmeurs doen dit om tekstgegevens te manipuleren en\u2026"
lastmod: 2024-02-18 23:09:01.513895
model: gpt-4-0125-preview
summary: "Het extraheren van substrings betekent het uithalen van specifieke delen\
  \ uit een tekenreeks. Programmeurs doen dit om tekstgegevens te manipuleren en\u2026"
title: Substrings extraheren
---

{{< edit_this_page >}}

## Wat & Waarom?
Het extraheren van substrings betekent het uithalen van specifieke delen uit een tekenreeks. Programmeurs doen dit om tekstgegevens te manipuleren en analyseren, of simpelweg om alleen relevante informatie aan gebruikers te tonen.

## Hoe:
In Elixir kun je substrings extraheren met behulp van de `String` module. Hier is hoe:

```elixir
str = "Hallo, Wereld!"

# Een substring extraheren op bereik
substr = String.slice(str, 7, 5)
IO.puts(substr)  # => Wereld

# Een substring tot het einde van de tekenreeks extraheren
substr_end = String.slice(str, 7)
IO.puts(substr_end)  # => Wereld!

# Een enkel karakter krijgen (ook technisch gezien een substring)
char = String.at(str, 1)
IO.puts(char)  # => e
```

Deze codefragmenten laten zien hoe je strings kunt extraheren op indexbereik, tot het einde van een tekenreeks, en het grijpen van een enkel karakter.

## Diepere Duik
Elixir's benadering van strings is beïnvloed door zijn Erlang-erfgoed, waarbij binaire codes worden gebruikt voor stringopslag. Extractie verschilt van talen zoals C die null-afgesloten strings gebruiken. Elixir substrings zijn UTF-8 en binaire-veilig, wat betekent dat ze karaktergrenzen respecteren.

In het verleden hadden verschillende programmeertalen en systemen hun eigen manieren om met tekenreeksen om te gaan, wat vaak leidde tot problemen met internationalisatie en geheugenbeheer. Elixir's op binaire gebaseerde strings bieden een universele en efficiënte methode voor stringmanipulatie.

Alternatieven in Elixir voor het extraheren van substrings buiten `String.slice` en `String.at` omvatten meestal regex-operaties of string patroonvergelijking, die beide krachtig kunnen zijn maar ook complexer.

Implementatiedetails zijn essentieel omdat het extraheren van substrings middelenvretend kan zijn, vooral bij verkeerde omgang met grote strings of het uitvoeren van talrijke bewerkingen. Elixir's functionele aard moedigt het verwerken van strings aan op een manier die gebruikmaakt van patroonvergelijking en recursie, wat kan helpen met prestaties en codehelderheid.

## Zie Ook
Voor verder lezen en meer gedetailleerde documentatie, kun je deze links bezoeken:

- Officiële `String` module documentatie van Elixir: [hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Begrijpen van binaire codes en strings in Elixir: [elixir-lang.org/getting-started/binaries-strings-and-char-lists.html](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- Elixir School's beschouwing van strings en patroonvergelijking: [elixirschool.com/en/lessons/basics/strings](https://elixirschool.com/en/lessons/basics/strings/) en [elixirschool.com/en/lessons/basics/pattern-matching](https://elixirschool.com/en/lessons/basics/pattern-matching/)
- Reguliere expressies in Elixir: [hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
