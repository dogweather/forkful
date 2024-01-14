---
title:    "PHP: Utskrift av feilsøkingsdata"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å skrive ut feilsøkingsutdata i PHP-kode er en nyttig måte å identifisere og løse problemer i koden din. Det kan hjelpe deg med å finne ut nøyaktig hvor feilen oppstår og hva som forårsaker den, noe som kan gjøre feilsøkingsprosessen mye enklere og mer effektiv.

## Hvordan gjøre det
For å skrive ut feilsøkingsutdata i PHP, kan du bruke funksjonen ```print_r()``` eller ```var_dump()```. Disse funksjonene skriver ut en detaljert representasjon av variabler eller objekter i koden din, noe som kan være spesielt nyttig når du jobber med komplekse datasett.

La oss si at du har en variabel som heter ```$navn``` og du vil vite hva verdien av denne variabelen er. Du kan enkelt skrive ut den ved å bruke ```print_r($navn)```. Dette vil skrive ut verdien av variabelen og dens datatype, noe som kan hjelpe deg å finne ut om det er der problemet ligger.

Et annet nyttig verktøy er ```error_log()``` funksjonen, som lar deg skrive ut egendefinerte feilmeldinger til en fil eller konsollen. Dette kan være svært nyttig når du jobber med større prosjekter og trenger å fange opp feilmeldinger fra forskjellige deler av koden din.

## Dypdykk
Å skrive ut feilsøkingsutdata er ikke bare nyttig når du må finne feil i koden din, det kan også hjelpe deg å forstå hvordan koden din fungerer og hvordan variabler og objekter endres underveis i programmet.

En annen ting å huske på er at oljen som brukes til å skrive ut feilsøkingsutdata kan påvirke ytelsen til koden din. Det er derfor viktig å ikke la utdaten være en del av koden din i produksjon, men heller fjerne den når du er ferdig med feilsøkingen.

## Se også
- [PHP Manual: Debugging](https://www.php.net/manual/en/debugger.php)
- [Debugging PHP with XDebug](https://xdebug.org/docs/remote)