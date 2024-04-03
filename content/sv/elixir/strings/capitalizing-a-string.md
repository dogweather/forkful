---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:51.369389-07:00
description: "Hur man g\xF6r: Elixir erbjuder ett enkelt s\xE4tt att g\xF6ra f\xF6\
  rsta bokstaven stor i str\xE4ngar med hj\xE4lp av sina inbyggda funktioner utan\
  \ behov av\u2026"
lastmod: '2024-03-13T22:44:37.550451-06:00'
model: gpt-4-0125-preview
summary: "Elixir erbjuder ett enkelt s\xE4tt att g\xF6ra f\xF6rsta bokstaven stor\
  \ i str\xE4ngar med hj\xE4lp av sina inbyggda funktioner utan behov av tredjepartsbibliotek."
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

## Hur man gör:
Elixir erbjuder ett enkelt sätt att göra första bokstaven stor i strängar med hjälp av sina inbyggda funktioner utan behov av tredjepartsbibliotek. Här är ett enkelt exempel:

```elixir
sträng = "elixir programmering"
kapitaliserad_sträng = String.capitalize(sträng)
IO.puts kapitaliserad_sträng
```

Utskrift:

```
Elixir programmering
```

I fall där man behöver mer kontroll eller mer komplex kapitaliseringslogik kan du kombinera olika Sträng-funktioner. Till exempel, om du vill göra första bokstaven stor i varje ord i en mening, kan du dela upp meningen i ord, göra första bokstaven stor i varje, och sedan sätta ihop dem igen:

```elixir
mening = "elixir är roligt"
kapitaliserad_mening = mening 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts kapitaliserad_mening
```

Utskrift:

```
Elixir Är Roligt
```

Medan Elixirs standardbibliotek täcker de flesta behov, för mer nyanserad textmanipulation, inklusive avancerad kapitalisering av strängar, kanske du utforskar tredjepartsbibliotek såsom Cldr för internationalisering, som kan erbjuda lokalspecifikt kapitaliseringsbeteende.
