---
title:    "Elixir: Lesing av kommandolinje-argumenter"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinjeargumenter er en nyttig ferdighet for enhver Elixir-programmerer. Det gir deg muligheten til å lage programmer som tar imot brukerinndata fra terminalen og gir mer fleksibilitet til applikasjonen din.

## Hvordan gjøre det

For å lese kommandolinjeargumenter i Elixir, bruker vi funksjonen `System.argv()`. Denne funksjonen returnerer en liste av argumenter som ble gitt da programmet ble kjørt. La oss se på et eksempel:

```Elixir
defmodule ArgumentReader do
  def read do
    args = System.argv()
    IO.inspect args
  end
end

ArgumentReader.read()
```

Hvis vi kjører dette programmet med kommandoen `elixir reader.exs first second third`, vil konsollen vise følgende output:

```bash
["first", "second", "third"]
```

I tillegg til å lese argumenter som er gitt ved kjøring, kan vi også ta imot brukerinput fra terminalen etter at programmet har startet. Dette gjøres ved å bruke funksjonen `IO.gets()`. La oss se på et eksempel:

```Elixir
defmodule TerminalInput do
  def prompt do
    IO.puts "Skriv inn navnet ditt: "
    name = IO.gets() # venter på brukerinndata
    IO.puts "Hei, #{name}!"
  end
end

TerminalInput.prompt()
```

Når vi kjører dette programmet, vil terminalen be oss om å skrive inn navnet vårt. Når vi har gjort det, vil programmet skrive ut en personlig hilsen med navnet vi har skrevet inn.

```bash
Skriv inn navnet ditt:
Elixir
Hei, Elixir!
```

## Dypdykk

I tillegg til å lese argumenter og ta imot brukerinndata, kan vi også håndtere ulike situasjoner som kan oppstå. For eksempel, hvis ingen argumenter blir gitt ved kjøring, vil funksjonen `System.argv()` returnere en tom liste `[]`. Dette kan føre til feil i koden vår, så det er viktig å håndtere dette tilfellet ved å sjekke om `args`-listen er tom og håndtere det deretter.

Vi kan også bruke funksjonene `List.first()` og `List.last()` for å hente ut den første og siste verdien i en liste. Dette kan være nyttig hvis vi for eksempel bare ønsker å lese den første argumenten og ignorere resten.

```Elixir
defmodule FirstArgumentReader do
  def read do
    args = System.argv()
    first_arg = List.first(args)

    IO.puts "Den første argumentet er: #{first_arg}"
  end
end

FirstArgumentReader.read()
```

I tillegg til de nevnte funksjonene, er det mange flere måter å håndtere kommandolinjeargumenter på i Elixir. Det kan være nyttig å utforske dokumentasjonen og prøve ut ulike løsninger for å finne den som passer best for ditt spesifikke brukstilfelle.

## Se også

- [Elixir Dokumentasjon: System](https://hexdocs.pm/elixir/System.html)
- [Elixir Dokumentasjon: IO](https://hexdocs.pm/elixir/IO.html)
- [Elixir Dokumentasjon: List](https://hexdocs.pm/elixir/List.html)
- [Elixir Dokumentasjon: Kernel](https://hexdocs.pm/elixir/Kernel.html)