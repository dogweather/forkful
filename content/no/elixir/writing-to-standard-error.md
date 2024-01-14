---
title:                "Elixir: Skriving til standard error"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error er et viktig aspekt av feilhåndtering i Elixir-programmering. Ved å skrive feilmeldinger til standard error, kan vi effektivt identifisere og håndtere eventuelle feil i koden vår. Derfor er det viktig å forstå og kunne implementere dette i våre programmer.

## Hvordan

Å skrive til standard error i Elixir er enkelt og kan gjøres ved hjelp av `IO.puts/1`-funksjonen. La oss se på et enkelt eksempel:

``` Elixir
defmodule Feilhåndtering do
  def del_tall(tall1, tall2) do
    if tall2 == 0 do
      IO.puts("Kan ikke dele på 0!")
    else
      IO.puts(tall1 / tall2)
    end
  end
end

Feilhåndtering.del_tall(10, 2)
Feilhåndtering.del_tall(10, 0)
```

I dette eksempelet deler vi tall1 med tall2, men hvis tall2 er 0, skriver vi en feilmelding til standard error i stedet for å prøve å dele på 0. Når vi kjører dette programmet, vil vi få følgende utskrift:

``` Elixir
5
Kan ikke dele på 0!
```

Som du kan se, kan vi enkelt skrive feilmeldinger til standard error ved å bruke `IO.puts/1`-funksjonen.

## Dypdykk

Hvis du vil ha mer kontroll over hvordan feilmeldinger håndteres, kan du bruke funksjonen `IO.write/2`. Denne funksjonen tar inn en IO-enhet og en liste av data som skal skrives til enheten. Dette gir oss muligheten til å skrive feilmeldinger til en spesifikk IO-enhet, som for eksempel en fil eller en logg.

Vi kan også bruke `Kernel.SpecialForms.raise/1`-funksjonen til å heve en feil og skrive en melding til standard error samtidig. La oss se på et eksempel:

``` Elixir
defmodule Feilhåndtering do
  def sjekk_tall(tall) do
    if is_integer(tall) do
      IO.puts("#{tall} er et heltall")
    else
      raise "Inndata må være et heltall"
    end
  end
end

Feilhåndtering.sjekk_tall(10)
Feilhåndtering.sjekk_tall("ti")
```

Når vi kjører dette programmet, vil vi få følgende utskrift:

``` Elixir
10 er et heltall
** (RuntimeError) Inndata må være et heltall
```

Som du kan se, blir feilmeldingen også skrevet til standard error ved hjelp av `raise/1`-funksjonen.

## Se også

- Elixir dokumentasjon for `IO`-modulen: https://hexdocs.pm/elixir/IO.html
- Blogginnlegg om feilhåndtering i Elixir: https://elixirschool.com/en/lessons/advanced/error-handling/