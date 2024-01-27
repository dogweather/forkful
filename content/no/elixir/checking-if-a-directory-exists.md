---
title:                "Sjekke om en mappe finnes"
date:                  2024-01-19
html_title:           "Arduino: Sjekke om en mappe finnes"
simple_title:         "Sjekke om en mappe finnes"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
I Elixir sjekker vi om en mappe eksisterer for å unngå feil ved filhåndtering. Det er essensielt for å sørge for at programmet fungerer glatt uten å krasje.

## Hvordan:
Det enkleste er å bruke `File` modulen:

```elixir
if File.dir?("sti/til/mappen") do
  IO.puts("Mappen eksisterer!")
else
  IO.puts("Mappen finnes ikke.")
end
```

Forventet output:
```
Mappen eksisterer!
# eller
Mappen finnes ikke.
```

Eller med `File.stat/2` for mer detaljer:

```elixir
case File.stat("sti/til/mappen") do
  {:ok, _stats} -> IO.puts("Mappen eksisterer!")
  {:error, :enoent} -> IO.puts("Mappen finnes ikke.")
  {:error, _reason} -> IO.puts("Det oppstod en ukjent feil.")
end
```

## Dypdykk
Før Elixir, i Erlang, brukte vi `filelib` biblioteket for lignende funksjonalitet. Elixir har gjort det mer brukervennlig med `File` modulen. Alternativt kan man bruke tredjepartsbiblioteker for mer avanserte filsystemoperasjoner. Når vi sjekker om en mappe eksisterer, bør vi også være klar over race conditions; en mappe kan slettes mellom sjekken og neste bruk, så en god praksis er å håndtere Exceptions som kan oppstå.

## Se også:
- Elixir's offisielle dokumentasjon for `File` modulen: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
- Erlang's `filelib` dokumentasjon for historisk kontekst: [http://erlang.org/doc/man/filelib.html](http://erlang.org/doc/man/filelib.html)
- Innføring i filhåndtering i Elixir: [https://elixir-lang.org/getting-started/io-and-the-file-system.html](https://elixir-lang.org/getting-started/io-and-the-file-system.html)
