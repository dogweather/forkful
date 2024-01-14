---
title:                "Elixir: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Skriving til standardfeil kan være en nyttig del av en Elixir programmerers verktøykasse. Det tillater deg å sende feilmeldinger eller annen informasjon som skal ses av brukeren i terminalen samtidig som koden kjører.

## Hvordan

For å skrive til standardfeil i Elixir, kan du bruke `IO.puts/2` eller `IO.write/2` funksjoner og passere `:stderr` som første argument. La oss se på et eksempel:

```Elixir
IO.puts(:stderr, "Dette er en feilmelding")
```

Dette vil skrive ut "Dette er en feilmelding" i standard feilstrømmen.

Du kan også kombinere dette med annen informasjon, for eksempel variabler eller funksjoner:

```Elixir
error_message = "En uventet feil oppstod"
IO.puts(:stderr, error_message <> " for bruker #{username}")
```

Dette vil skrive ut "En uventet feil oppstod for bruker Jane" hvis `username` variabelen er satt til "Jane".

## Dypdykk

En ting å merke seg er at skriving til standardfeil ikke vil stoppe programmet hvis det oppstår en feil. Det vil bare skrive ut informasjonen til brukeren. Du kan også se etter feil som oppstår og skrive dem til standardfeil ved hjelp av `try/catch` blokker:

```Elixir
try do
  # Kode som kan føre til en feil
catch
  message ->
    IO.puts(:stderr, message)
end
```

Dette vil fange en eventuell feil som oppstår i `try` blokken og skrive den til standard feilstrømmen.

## Se Også

- [Elixir IO modul](https://hexdocs.pm/elixir/IO.html)
- [Elixir Try/Catch dokumentasjon](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#try/1)
- [Elixir Error modul](https://hexdocs.pm/elixir/Error.html)
- [Elixir Exception dokumentasjon](https://hexdocs.pm/elixir/Exception.html)