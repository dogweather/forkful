---
title:                "Gör om en sträng till versaler"
aliases:
- /sv/elixir/capitalizing-a-string/
date:                  2024-02-03T19:04:51.369389-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att göra första bokstaven i en sträng stor handlar om att omvandla strängens första bokstav till versal samtidigt som man säkerställer att resten av bokstäverna är i gemener. Denna åtgärd är vanligt nödvändig för att formatera användarinmatning eller visa text i användargränssnitt, där konsekvens och läsbarhet är viktiga.

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
