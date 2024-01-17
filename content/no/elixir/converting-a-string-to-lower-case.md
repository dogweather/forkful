---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Elixir: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?

Konvertering av en streng til små bokstaver er en vanlig operasjon i programmere. Dette innebærer å ta en tekststreng og endre alle store bokstaver til små. Dette gjøres vanligvis for å standardisere data og gjøre det lettere å sammenligne og manipulere tekst.

# Slik gjør du det:

```Elixir
iex> String.downcase("Hei Verden")
"hei verden"
```

I dette eksempelet bruker vi den innebygde funksjonen `downcase` fra `String` modulen for å konvertere teksten "Hei Verden" til små bokstaver. Resultatet blir "hei verden".

# Utforsk videre:

Historisk sett har konvertering av strenger til små bokstaver vært viktig for programmer som arbeider med ASCII-tegnsettet. Alternativt kan man også bruke `String.trim` for å fjerne eventuelle mellomrom og `String.upcase` for å gjøre alle bokstavene store.

I Elixir er strenger basert på Unicode-tegnsettet, så det er også viktig å være oppmerksom på unicode-tilpasning og håndtering av spesialtegn når man jobber med tekster. Elixir har innebygde funksjoner for dette, som `String.upcase/1`, `String.downcase/1` og `String.titlecase/1`.

# Se også:

- Elixir dokumentasjon for mer informasjon om `String` modulen: https://hexdocs.pm/elixir/String.html
- Offisiell Elixir hjemmeside: https://elixir-lang.org/