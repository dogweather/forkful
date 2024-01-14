---
title:    "Elixir: Sammenføyning av strenger"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hvorfor
Å slå sammen strenger er en viktig del av å jobbe med data i Elixir. Det lar deg kombinere tekst og variabler for å lage mer dynamisk og spesifikk informasjon som kan brukes i apper, nettsider og mer.

# Hvordan du gjør det
For å slå sammen strenger i Elixir, bruker du operatøren `<>` mellom to verdier. Den første verdien er alltid en streng, og den andre kan være en annen streng, et tall eller en variabel.

```elixir
string1 = "Hei"
string2 = "verden!"
output = string1 <> string2
IO.puts(output)
```

Dette vil produsere følgende resultat:

```
Hei verden!
```

Du kan også bruke funksjonen `to_string()` for å konvertere tall eller variabler til strenger og deretter slå dem sammen. For eksempel:

```elixir
number = 42
output = "Svaret på alt er " <> to_string(number)
IO.puts(output)
```

Dette vil gi følgende resultat:

```
Svaret på alt er 42
```

Vær oppmerksom på at rekkefølgen på verdiene kan påvirke resultatet av sammenføyningen. Forsøk å bytte plass på verdiene i eksemplene ovenfor for å se hva som skjer!

# Dypdykk
Når du slår sammen strenger i Elixir, opprettes det faktisk en ny streng i minnet. Derfor er det viktig å ikke slå sammen en stor mengde strenger i en løkke eller rekursiv funksjon, da dette kan føre til problemer med minneutnyttelse.

Du kan også bruke funksjonen `++` for å legge til en verdi til slutten av en streng, og `--` for å fjerne en verdi fra en streng. Dette kan være nyttig når du jobber med lister av verdier som du ønsker å konvertere til en streng.

# Se også
- [Elixir dokumentasjon om strenger](https://elixir-lang.org/getting-started/types.html#strings)
- [Elixir string-håndteringsfunksjoner](https://hexdocs.pm/elixir/String.html)
- [En guide til Elixir-programmering for nybegynnere](https://www.freecodecamp.org/news/elixir-tutorial-a-scalable-backend-programming-language-ef4c8a79c1b3/)