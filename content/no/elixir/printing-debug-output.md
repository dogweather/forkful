---
title:    "Elixir: Utskrift av feilsøkingsutdata"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du skriver Elixir kode, er det viktig å ha en måte å feilsøke på. Å legge til debug-utskrift i koden din er en enkel og effektiv måte å finne ut hva som skjer bak kulissene under kjøringen av programmet ditt.

## Slik gjør du det

Det er enkelt å legge til debug-utskrift i Elixir-kode. Bruk bare funksjonen `IO.inspect()` for å skrive ut verdier til konsollen. La oss se på et eksempel:

```elixir
defmodule HelloWorld do
    def main do
        name = "Verden"
        IO.inspect(name)
    end
end

HelloWorld.main()
```

Dette vil gi følgende utskrift i terminalen:

```
"Verden"
```

Som du kan se, skriver `IO.inspect()` ut verdien av variabelen vi har definert. Du kan også legge til en tittel for å gjøre utskriften mer meningsfull ved å legge til en komma-separert liste som det andre argumentet i `IO.inspect()`:

```elixir
defmodule HelloWorld do
    def main do
        name = "Verden"
        IO.inspect(name, label: "Navn")
    end
end

HelloWorld.main()
```

Dette vil gi følgende utskrift:

```
Navn: "Verden"
```

Du kan også legge til flere verdier i utskriften ved å bruke flere argumenter:

```elixir
defmodule Person do
    def main do
        name = "Maria"
        age = 25
        IO.inspect(name, label: "Navn", age, label: "Alder")
    end
end

Person.main()
```

Dette vil gi følgende utskrift:

```
Navn: "Maria", Alder: 25
```

## Dykk dypere

Det finnes flere nyttige alternativer til `IO.inspect()` som kan hjelpe deg med å feilsøke koden din. Her er noen av de viktigste:

- `IO.inspect(term, opts \\ [])`: Dette er den grunnleggende funksjonen for å skrive ut verdier i terminalen.

- `IO.inspect(term, label: "Title")`: Gir en tittel for utskriften av `term`.

- `IO.inspect(term, width: 5)`: Begrenser utskriften til en bestemt bredde (her 5).

- `IO.inspect(term, charlists: :as_lists)`: Gjør at utskriften av charlists blir mer lesbar ved å vise dem som lister i stedet for å vise tegnene som tall.

- `IO.inspect(term, limit: 10)`: Begrenser antall elementer som skrives ut for en liste, tuple eller map, for å unngå at utskriften blir for lang.

For mer detaljert informasjon om alle alternativene for `IO.inspect()`, kan du se [offisiell dokumentasjon](https://hexdocs.pm/elixir/master/IO.html#inspect/2).

## Se også

- [Elixir offisiell dokumentasjon](https://elixir-lang.org/getting-started/introduction.html)
- [Anbefalte ressurser for å lære Elixir](https://serokell.io/blog/best-resources-to-learn-elixir)
- [Introduksjon til Elixir for nybegynnere](https://medium.com/swlh/an-introduction-to-elixir-for-beginners-b3c11e31b2ec)