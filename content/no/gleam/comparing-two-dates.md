---
title:    "Gleam: Sammenligning av to datoer"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med datoer og tider i din Gleam-kode, vil du på et tidspunkt trenge å sammenligne to forskjellige datoer. Dette kan være for å sjekke om en hendelse fant sted før eller etter en annen, eller bare for å sammenligne to datoer for å se hvilken som kommer først. Uansett hva grunnen måtte være, er det viktig å vite hvordan man sammenligner to datoer i Gleam for å sikre nøyaktighet og effektivitet i koden din.

## Hvordan gjøre det

For å sammenligne to datoer i Gleam må vi først opprette to `DateTime`-objekter som representerer de to datoene vi vil sammenligne. Dette kan gjøres ved å bruke `DateTime.from_timestamp`-funksjonen og gi den et Unix-tidspunkt for den spesifikke datoen. Deretter kan vi bruke `DateTime.compare`-funksjonen til å sammenligne de to datoene. Her er et eksempel:

```
Gleam
let date1 = DateTime.from_timestamp(1589904000) // 20. mai 2020
let date2 = DateTime.from_timestamp(1590950400) // 1. juni 2020
let comparison = DateTime.compare(date1, date2)
```

I dette eksempelet har vi opprettet to datoer som representerer 20. mai 2020 og 1. juni 2020. Deretter har vi brukt `DateTime.compare`-funksjonen til å sammenligne de to datoene. Denne funksjonen returnerer en `Comparison`-type som kan være `Equal`, `Less` eller `Greater`, avhengig av hvilken dato som kommer først. I skriptkoden vil `comparison`-variabelen være `Greater` fordi 1. juni kommer etter 20. mai.

## Dypere dykking

Når du sammenligner to datoer i Gleam, er det viktig å vite om tidssoner spiller en rolle. Dette er spesielt viktig hvis du jobber med datoer fra forskjellige tidssoner. I så fall kan det hende at du må konvertere datoene til en felles tidssone før du sammenligner dem. Du bør også være oppmerksom på eventuelle problemer med skuddårsdager eller ulike format på datoene, som kan påvirke resultatet av sammenligningen.

Det er også verdt å merke seg at Gleam ikke har en innebygd funksjon for å sammenligne tider. Hvis du også trenger å sammenligne klokkeslett, kan du konvertere hver dato til et `Time`-objekt og bruke `Time.compare`-funksjonen på samme måte som vi gjorde for datoene.

## Se også

- [DateTime-modulen i Gleam](https://gleam.run/modules/datetime.html)
- [Gleam-dokumentasjonen for DateTime-funksjoner](https://gleam.run/modules/datetime.html#functions)
- [Eksempelkode for å sammenligne to datoer i Gleam](https://github.com/pblatteier/gleam-date-compare-example)