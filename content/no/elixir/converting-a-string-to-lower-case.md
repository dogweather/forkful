---
title:    "Elixir: Konvertering av en streng til små bokstaver"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hvorfor

I Elixir programmering, er det ofte nødvendig å konvertere strenger til små bokstaver for å sikre en ensartet formatering av data. Dette kan være spesielt viktig når det gjelder å sammenligne strenger eller filtrere data. Ved å konvertere en streng til små bokstaver, eliminerer du muligheten for feil på grunn av forskjellige formater, og sikrer nøyaktige resultater.

# Hvordan gjøre det

For å konvertere en streng til små bokstaver i Elixir, kan du bruke den innebygde funksjonen `String.downcase/1` som tar inn en streng som argument og returnerer den samme strengen med alle bokstaver konvertert til små.

```Elixir
iex> String.downcase("HELLO")
"hello"
```

Det er også mulig å bruke operatøren `|>` for å kjede funksjoner sammen og gjøre koden mer lesbar.

```Elixir
iex> "HELLO" |> String.downcase()
"hello"
```

Det er viktig å merke seg at funksjonen `String.downcase/1` bare konverterer engelske bokstaver til små. Hvis du trenger å håndtere internasjonale tegn, kan du bruke funksjonen `String.downcase/2` som tar inn to argumenter: en streng og et språk-metode.

```Elixir
iex> String.downcase("Æ Ø Å", :icu)
"æ ø å"
```

# Dykk dypere

Hvis du ønsker å forstå mer om hvordan `String.downcase/1` fungerer, kan du ta en titt på koden bak det. Funksjonen bruker en kombinasjon av regulære uttrykk og Erlang funksjoner for å konvertere strengen. Hvis du vil lære mer om hvordan regulære uttrykk fungerer i Elixir, kan du lese [dette innlegget](https://eltalks.com/bli-komfortabel-med-regulaere-uttrykk-i-elixir/).

# Se også

- [String module documentation](https://hexdocs.pm/elixir/String.html)
- [Elixir School: Strings](https://elixirschool.com/en/lessons/basics/built-in-types/#strings)
- [ElixirForum: Converting strings to lower case](https://elixirforum.com/t/converting-text-to-all-uppercase-lowercase/9775)