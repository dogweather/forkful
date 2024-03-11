---
date: 2024-01-20 17:47:09.172167-07:00
description: "\xC5 finne lengden p\xE5 en streng betyr \xE5 telle antall tegn den\
  \ har. Programmerere gj\xF8r dette for \xE5 validere input, begrense st\xF8rrelsen,\
  \ eller optimalisere\u2026"
lastmod: '2024-03-11T00:14:13.959709-06:00'
model: gpt-4-1106-preview
summary: "\xC5 finne lengden p\xE5 en streng betyr \xE5 telle antall tegn den har.\
  \ Programmerere gj\xF8r dette for \xE5 validere input, begrense st\xF8rrelsen, eller\
  \ optimalisere\u2026"
title: "Finn lengden p\xE5 en streng"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å finne lengden på en streng betyr å telle antall tegn den har. Programmerere gjør dette for å validere input, begrense størrelsen, eller optimalisere ytelsen.

## Hvordan gjøre det:

```elixir
# Finn lengden på en streng med String.length/1
str = "Hei, Norge!"
length = String.length(str)
IO.puts(length)
```
Output:
```
11
```

## Dypdykk

Tilbake i tiden, før Unicode, var strenglengde og antall byte ofte det samme. I Elixir jobber `String.length/1` med Unicode og gir antall grafemer, ikke byte. Skal du telle byte, bruk `byte_size/1`. Alternativer inkluderer å bruke regex eller egendefinerte funksjoner, men `String.length/1` er mest effektiv for Elixir-strenger.

Implementasjonsmessig sørger Elixir for at `String.length/1` korrekt håndterer Unicode og gir et nøyaktig antall grafemer, som kan være mer enn antall bytes på grunn av hvordan noen tegn lagres.

## Se også:

- Elixir's offisielle [String-modul dokumentasjon](https://hexdocs.pm/elixir/String.html)
- [The Unicode Consortium](https://home.unicode.org/) for mer utfyllende info om Unicode håndtering.
- [RegExr](https://regexr.com/) for å leke med regular expressions og strenger.
