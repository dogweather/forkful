---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Elm: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle man bry seg om å skrive ut feilsøkingsinformasjon når man koder? Vel, det kan virke tidkrevende og unødvendig, men faktum er at det kan være en uvurderlig ressurs når du finner bugs og feil i koden din. Det kan hjelpe deg med å identifisere hvor problemet oppstår, og dermed gjøre debugging og troubleshooting mye enklere.

## Slik gjør du det

For å skrive ut feilsøkingsinformasjon i Elm, kan du bruke funksjonen `Debug.log` som tar inn en streng som beskriver informasjonen du ønsker å skrive ut, og en variabel som inneholder den aktuelle informasjonen. La oss si at vi har en funksjon som beregner summen av to tall:

```Elm
add x y =
    Debug.log "Funksjonen add ble kalt" (x + y)
```

I dette eksempelet vil vi skrive ut en beskjed som forteller oss at funksjonen `add` ble kalt, og deretter skrive ut summen av variablene `x` og `y`. Når du kjører koden, vil følgende vises i konsollen:

```
Funksjonen add ble kalt: 9
```

Dette gjør det enkelt å bekrefte at funksjonen ble kalt med de riktige verdiene, og at summen ble beregnet riktig.

## Dykke dypere

I tillegg til `Debug.log` finnes det også andre funksjoner du kan bruke for å skrive ut feilsøkingsinformasjon, som for eksempel `Debug.logMany` som lar deg skrive ut flere variabler og `Debug.todo` som lar deg markere steder i koden som ennå ikke er implementert.

Det er også verdt å merke seg at `Debug`-modulen er utformet slik at funksjonene ikke vil bli kjørt i produksjonsmiljøet, så du trenger ikke bekymre deg for at feilsøkingsinformasjonen vil påvirke ytelsen til applikasjonen din.

## Se også

- [Debugging in Elm](https://guide.elm-lang.org/debugging/)
- [The Power of Elm's Debug Module](https://medium.com/javascript-inside/the-power-of-elms-debug-module-37bb47404ac8)
- [Debugging in Elm with Chrome DevTools](https://www.brianthicks.com/post/2016/10/28/debugging-elm-with-chrome-devtools/)