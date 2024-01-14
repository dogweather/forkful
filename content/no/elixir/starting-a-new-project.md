---
title:                "Elixir: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvis du er en programmerer, enten nybegynner eller erfaren, som er på utkikk etter et nytt prosjekt å dykke inn i, kan Elixir være et spennende valg. Dette funksjonelle programmeringspråket kombinerer funksjonalitet og skalerbarhet, noe som gjør det til et kraftig verktøy for å bygge pålitelige og fleksible applikasjoner.

# Hvordan

For å starte et nytt prosjekt i Elixir, må du først installere språket på datamaskinen din. Deretter kan du følge disse trinnene for å komme i gang:

```
Elixir new prosjektnavn
cd prosjektnavn
mix test
```

Dette vil opprette en ny mappe med prosjektnavnet ditt, navigere deg til mappen og kjøre en test for å sikre at alt fungerer som det skal.

For å legge til avhengigheter i prosjektet ditt, kan du redigere filen `mix.exs` og legge til nødvendige biblioteker under `deps`. Når du har gjort endringer, kjør `mix deps.get` for å laste ned de nye avhengighetene.

For å skrive kode i Elixir, kan du bruke enten en fil med `.ex`-utvidelse eller kjøre interactive mode ved å skrive `iex` i terminalen. Her er et eksempel på Elixir-kode, som definerer en funksjon som beregner summen av to tall:

```
defmodule Kalkulator do
  def sum(a, b) do
    a + b
  end
end

IO.puts Kalkulator.sum(2, 3)
# Output: 5
```

# Dykk dypere

Elixir har et rikt økosystem med mange løsninger for å håndtere forskjellige funksjoner, som webutvikling, databehandling og parallelle programmer. Ta deg tid til å utforske forskjellige biblioteker og ressurser som er tilgjengelige for å hjelpe deg med ditt prosjekt. Du kan også lære mer om Elixir ved å sjekke ut offisiell dokumentasjon og delta i det aktive samfunnet på nettet.

# Se også

- [Elixir Official Documentation](https://elixir-lang.org/docs.html)
- [Awesome Elixir](https://github.com/h4cc/awesome-elixir)
- [Elixir Forum](https://elixirforum.com/)