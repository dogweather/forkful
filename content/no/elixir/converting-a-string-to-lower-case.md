---
title:                "Konvertere en streng til små bokstaver"
date:                  2024-01-20T17:38:28.696065-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en streng til små bokstaver"

category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Konvertering av strenger til små bokstaver betyr å endre alle tegn i en tekststreng til deres små bokstavversjon. Programmerere gjør dette for å standardisere data, for eksempel for søkeoptimalisering eller for å sammenligne strenger uavhengig av bokstavstørrelse.

## How to:
I Elixir bruker vi `String.downcase/2` for å konvertere strenger til små bokstaver. Her er et eksempel:

```elixir
original = "Hei Verden!"
lowercased = String.downcase(original)
IO.puts lowercased
```

Forventet utskrift:

```
hei verden!
```

Du kan også angi et bestemt språklokale for å sikre at spesifikke tegnsettsregler følges:

```elixir
norwegian_text = "ÆØÅ Ære være Norge!"
lowercased_norwegian = String.downcase(norwegian_text, :norwegian)
IO.puts lowercased_norwegian
```

Forventet utskrift:

```
æøå ære være norge!
```

## Deep Dive
Funksjonen `String.downcase/2` har eksistert siden de tidlige dagene av Elixir og gjør bruk av Unicode for å sikre riktig håndtering av forskjellige språk. Alternativene til `String.downcase/2` inkluderer manuell manipulering av strengene, men dette er sjelden effektivt og kan føre til feil i språk med komplekse tegnsettsregler, som tyrkisk.

Implementeringen bruker utfordringen med Unicode normalization, som transformere tekst til en stabil form som kan sammenlignes binært. Denne prosessen håndterer ikke bare ASCII-tegn, men også internasjonale tegn og symboler korrekt.

Elixir's `String` modul bruker Erlang's `:unicode` modul bak kulissene, noe som gir ytterligere robusthet i håndteringen av internasjonalisert tekst.

## See Also
- [Elixir String Module Documentation](https://hexdocs.pm/elixir/String.html)
- [Unicode Normalization Forms](http://unicode.org/reports/tr15/)
- [Erlang's :unicode Module Documentation](http://erlang.org/doc/man/unicode.html)
