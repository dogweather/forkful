---
title:                "Gleam: Utskrift av feilsøkingsdata"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hvorfor

Det å skrive ut feilsøkingsutdata kan være en veldig nyttig måte å forstå hva som skjer i koden din. Det kan hjelpe deg med å finne feil og forbedre effektiviteten til programmet ditt.

## Hvordan du gjør det

Du kan enkelt skrive ut feilsøkingsutdata i Gleam ved å bruke `log()` funksjonen. Denne funksjonen tar imot en hvilken som helst verdi og skriver den ut til konsollen. La oss se på et eksempel:

```Gleam
let navn = "Per"
log(navn)
```

Dette vil skrive ut "Per" i konsollen når programmet kjører. Du kan også skrive ut flere verdier ved å separere dem med komma, for eksempel `log(navn, alder)`.

## Dykk dypere

For å utnytte fullt ut potensialet til å skrive ut feilsøkingsutdata, kan du også bruke `debug()` funksjonen. Denne funksjonen tar imot en funksjon som parameter, og vil kjøre den og skrive ut resultatet til konsollen. Dette er spesielt nyttig når du jobber med funksjonelle programmeringselementer som høyere ordens funksjoner. La oss se på et eksempel:

```Gleam
let liste = [1, 2, 3, 4]
let dobbel = fn(x) { x * 2 }
let dobbeltListe = debug(map(dobbel, liste))
```

Her vil `debug()` kjøre `map()` funksjonen og skrive ut resultatet av hvert element i listen. Dette kan hjelpe deg med å forstå hvordan funksjonen fungerer og om det er eventuelle feil i koden din.

# Se også

- [Gleam Feilsøking Dokumentasjon](https://gleam.run/book/tour/debugging.html)
- [10 Tips for Effektiv Feilsøking i Gleam](https://medium.com/@user22829148/10-tips-for-effective-debugging-in-gleam-94f1fec099b)