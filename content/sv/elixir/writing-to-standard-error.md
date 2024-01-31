---
title:                "Skriva till standardfel"
date:                  2024-01-19
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"

category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva till standard error (stderr) innebär att skicka felmeddelanden och diagnostik separat från standard output (stdout). Det hjälper till att separera programdata från fel och loggning, vilket gör det enklare att analysera problem.

## Hur gör man:
```elixir
# Skicka ett enkelt meddelande till stderr
IO.puts(:stderr, "Ett fel inträffade!")

# Använda :io.format/3 för formaterade meddelanden
IO.format(:stderr, "Fel: ~s~n", ["Något gick snett"])

# Logga ett fel med en stack trace
try do
  raise "Ett undantag!"
rescue
  exception -> 
    IO.puts(:stderr, Exception.format(:error_report, exception))
end
```

Exempelutdata:
```
Ett fel inträffade!
Fel: Något gick snett
=ERROR REPORT==== 24-Feb-2023::12:31:15 ===
** (RuntimeError) Ett undantag!
```

## Djupdykning
Io-modulen i Elixir har hanterat stderr sedan språkets tidiga dagar, följer konventionen från erlang och andra UNIX-baserade system. Alternativ till stderr inkluderar skriva loggfiler och använda externa loggtjänster. Stderr-strömmen är vanligen obuffrad, vilket innebär att utdata skrivs direkt utan fördröjning, till skillnad från stdout som kan buffras.

## Se även
- Elixir's officiella dokumentation för IO-modulen: https://hexdocs.pm/elixir/IO.html
- Erlang's :io bibliotek, som Elixir's IO bygger på: http://erlang.org/doc/man/io.html
- UNIX-standard för standard streams (stdin, stdout, stderr): https://en.wikipedia.org/wiki/Standard_streams
