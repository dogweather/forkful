---
title:                "Gleam: Utskrift av feilsøkningsutdata"
simple_title:         "Utskrift av feilsøkningsutdata"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive koden din kan ofte føles som en tålmodighetsprøve, spesielt når du støter på feil og feil i koden din. En måte å sikre at koden din fungerer slik den skal, er å bruke debugging. Printing av debug-output kan hjelpe deg med å identifisere og løse feil i koden din, og derfor er det et viktig verktøy for alle programmører.

## Hvordan

For å skrive debug-utdata i Gleam, bruker du en innebygd funksjon kalt ```debug.print``` og leverer en verdi som en parameter. La oss si at du har en funksjon som legger sammen to tall:

```Gleam
fun sum(x, y) {
    let result = x + y
    debug.print(result)
}
```

Når du kaller denne funksjonen, vil den skrive ut resultatet i terminalen din. For eksempel, hvis du kaller ```sum(2, 3)```, vil output være ```5```. Dette kan hjelpe deg med å bekrefte at koden din kjører som forventet.

## Dypdykk

Etter å ha printet debug-utdata, kan det være nyttig å formatere og organisere output for å gjøre det lettere å lese og analysere. Gleam tilbyr flere funksjoner for å hjelpe deg med dette, som for eksempel ```debug.inspect``` for å inspisere komplekse datastrukturer og ```debug.format``` for å formatere utdataen etter dine behov.

I tillegg kan du også bruke logging-biblioteker som f.eks. ```gleam-logger``` for å lagre debug-utdata til en loggfil, noe som kan være nyttig for lengre og mer komplekse koder.

## Se også

- [Offisiell Gleam dokumentasjon for debugging](https://gleam.run/book/tour/debugging.html)
- [Gleam Logger bibliotek](https://github.com/gleam-lang/gleam-logger)
- [Eksperimentell Gleam feature for å skrive loggdata](https://github.com/gleam-lang/gleam/pull/618)