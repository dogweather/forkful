---
title:    "Elixir: Utskrift av feilsøkingsutdata"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Hvorfor Printe Debug Output?

Når vi utvikler en programvare, er det alltid en mulighet for at ting ikke går som planlagt. Kanskje vår kode ikke kjører som forventet, eller kanskje det er en uventet feil i systemet. I slike situasjoner er det viktig å kunne feilsøke og finne ut hva som skjer. En måte å gjøre dette på er å bruke debug output. Dette er en måte å skrive ut relevant informasjon mens koden kjører, slik at vi kan analysere og forstå hva som skjer.

# Hvordan Skrive Ut Debug Output

For å skrive ut debug output i Elixir, kan vi bruke funksjonen `IO.inspect`. Denne funksjonen tar inn et argument og skriver det ut til konsollen. La oss se på et eksempel:

```Elixir
IO.inspect("Hei verden!")
```

Dette vil skrive ut `"Hei verden!"` til konsollen. Vi kan også velge å skrive ut flere variabler på samme tid:

```Elixir
navn = "Nina"
alder = 28
IO.inspect(navn, alder)
```

Dette vil skrive ut `"Nina"` og `28` til konsollen på samme linje. Vi kan også bruke `IO.puts` for å skrive ut informasjon på separate linjer.

# Dykk Dypere Inn i Debug Output

Det finnes flere måter å bruke debug output på for å få en bedre forståelse av hva som skjer i koden. For eksempel kan vi bruke `IO.inspect` i kombinasjon med betingede uttrykk for å kun skrive ut informasjon når en bestemt betingelse er oppfylt. Vi kan også bruke `IO.inspect` i løkker for å få en oversikt over verdier som endres underveis.

Det er også viktig å forstå hvordan debug output kan hjelpe oss med å finne og fikse feil i koden. Ved å skrive ut relevante variabler og verdier, kan vi spore opp hvor feilen skjer, og deretter rette den.

# Se Også

- [Elixir dokumentasjon for IO.inspect](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [En artikkel om debugging i Elixir](https://elixir-lang.org/blog/2018/06/05/debugging-in-elixir/)
- [En video om debugging i Elixir](https://www.youtube.com/watch?v=3ELY_NFEPa4)