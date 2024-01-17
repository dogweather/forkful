---
title:                "Skriving til standardfeil"
html_title:           "Elixir: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Hva og hvorfor?
Å skrive til standard error er en måte for utviklere å kommunisere feil, advarsler eller annen informasjon fra et program til brukeren. Dette lar oss skille mellom vanlig program output og feilmeldinger, som kan være nyttig for å feilsøke og forbedre programmet vårt.

Hvordan å:
For å skrive til standard error i Elixir, kan vi bruke funksjonen `IO.puts/2` sammen med `:stderr` som det første argumentet. Se eksemplet nedenfor:

```Elixir
IO.puts(:stderr, "Dette er en feilmelding.")
```

Dette vil gi følgende utgang i terminalen:
```
Dette er en feilmelding.
```

Dypdykk:
Skriving til standard error har vært en vanlig praksis blant utviklere siden 1970-tallet. I Elixir, er dette forvandlet til et modul som lar oss skrive til både standard output og standard error. Alternativt, kan vi også bruke `IO.puts/2` med `:stderr` og `:stdio` for å skrive til begge utgangene. Implementeringsmessig, blir standard error som regel sendt til standard output på grunn av samme logiske konsoll som brukes for begge.

Se også:
Mer informasjon om skriving til standard error i Elixir kan bli funnet på dokumentasjonen her: https://hexdocs.pm/elixir/IO.html#puts/2