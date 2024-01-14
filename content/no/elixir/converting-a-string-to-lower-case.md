---
title:    "Elixir: Oversette en streng til små bokstaver"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Hvorfor 

Å konvertere en streng til små bokstaver kan være nyttig av flere grunner. Noen ganger er det nødvendig for å sammenligne tekst eller for å formatere data. Uansett grunn kan prosessen være enkel og effektiv med Elixir.

# Hvordan

For å konvertere en streng til små bokstaver i Elixir, bruker du funksjonen `String.downcase/1`. Den tar en streng som argument og returnerer en ny streng med kun små bokstaver.

```Elixir
String.downcase("HEI") 
# => "hei"
```

Hvis du trenger å konvertere en streng som allerede er i en variabel, kan du bruke `String.downcase!/1` som endrer verdien av variabelen direkte.

```Elixir
tekst = "DENNE STRENGEN VIL BLI KONVERTERT"
tekst |> String.downcase!()
tekst # => "denne strengen vil bli konvertert"
```

Det er også mulig å konvertere en liste med strenger til små bokstaver ved å bruke `Enum.map/2` funksjonen og `String.downcase/1` på hvert element i listen.

```Elixir
liste = ["ENEBAKK", "OSLO", "BERGEN"]
liste |> Enum.map(&String.downcase/1)
# => ["enebakk", "oslo", "bergen"]
```

# Dypdykk

I Elixir er strenger representert som binære data. Derfor er ikke `String.downcase/1` bare en enkel funksjon som endrer bokstavene til små, det er en mer kompleks prosess.

Når bokstavene blir konvertert, blir de først separert fra hverandre og sammenlignet med en liste over store og små bokstaver. Deretter blir de relevante endringene gjort for å konvertere dem til små bokstaver. Til slutt blir de satt sammen igjen til en ny streng.

Det er også verdt å merke seg at denne prosessen er språkuavhengig, noe som betyr at den vil fungere for alle språk som bruker alfabetet.

# Se Også

- [Elixir's offisielle dokumentasjon om strenger](https://elixir-lang.org/getting-started/string.html)
- [Elixir Forum - Hvor åpne diskusjoner og hjelp kan bli funnet](https://elixirforum.com/)