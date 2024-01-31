---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Skriving til standard error (stderr) er å sende feilmeldinger til en egen strøm. Programmere bruker det for å skille vanlig utdata fra feilmeldinger.

## How to:
For å skrive til stderr i Ruby, bruk `$stderr.puts` eller `STDERR.puts`.

```Ruby
puts "Dette er standard utdata."
$stderr.puts "Dette er en feilmelding."
```

Resultat:
```
Dette er standard utdata.
Dette er en feilmelding.
```

Eller skriv direkte til strømmen:

```Ruby
$stderr.write "Opps, noe gikk galt!"
```

Resultat:
```
Opps, noe gikk galt!
```

## Deep Dive
Skriverhistorien i Unix-systemer la grunnlaget for stderr. Ruby, som andre høynivåspråk, arver denne mekanismen. Alternativer inkluderer logging-biblioteker som `Logger` eller å omdirigere `stderr` til en fil for feilsøking. Intern implementasjon bruker global variabel `$stderr`, som er synonymt med `STDERR` og kan omdefineres.

## See Also
- [Ruby-dokumentasjon om IO](https://ruby-doc.org/core/IO.html)
- [Unix Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)
