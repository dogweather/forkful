---
title:    "Gleam: Skriver til standard feil"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error er en vanlig praksis i programmering, spesielt når man jobber med feilsøking og debugging. Det lar deg sende pålitelig og presis informasjon om eventuelle feil eller unntak som kan oppstå under kjøringen av programmet ditt.

## Hvordan du gjør det

For å skrive til standard error, kan du bruke den innebygde funksjonen `io.format_error/2` i Gleam. La oss se på et eksempel på hvordan vi kan bruke denne funksjonen:

```
Gleam
let error_message = "En feil har oppstått"
let error_code = 500
io.format_error("Feilkode: {} - {}", [error_code, error_message])
```

Dette vil skrive følgende til standard error:

```
Feilkode: 500 - En feil har oppstått
```

Som du kan se, kan du enkelt sette inn variabler inne i teksten ved å bruke krøllparenteser `{}`, som vil bli erstattet med verdien til variabelen. Dette gjør det enkelt å tilpasse meldingen basert på den aktuelle feilen.

## Dykk ned i detaljene

Det er viktig å merke seg at når du skriver til standard error, vil dette skje i sanntid mens programmet kjører. Dette betyr at det kan påvirke ytelsen, spesielt hvis du bruker det i en løkke eller andre raske operasjoner. Derfor bør du bare bruke denne metoden når det er nødvendig, for eksempel under feilsøkingsprosessen.

Du kan også bruke `io.format_error/2` for å skrive til standard output ved å passere `io.stdout` som første argument. Men det er viktig å merke seg at standard output blir buffret, så utskriften din vil ikke vises umiddelbart.

## Se også

- [Offisiell Gleam-dokumentasjon om io-modulen](https://gleam.run/documentation/std/io)
- [Artikkel om debugging i Gleam](https://dev.to/happi/debugging-in-gleam-15gj)
- [Eksempelkode som viser bruk av `io.format_error/2`](https://github.com/gleam-lang/gleam/blob/main/examples/error_logs_error_handler.gleam)