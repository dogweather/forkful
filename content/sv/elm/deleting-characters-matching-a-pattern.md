---
title:                "Radera tecken som matchar ett mönster"
html_title:           "Elm: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ta bort tecken som matchar ett mönster, även känt som "deleting characters matching a pattern" på engelska, är en vanlig operation bland programmerare. Det gör att man kan filtrera eller rensa bort vissa tecken från en textsträng baserat på ett visst mönster. Detta kan vara användbart när man vill manipulera data eller extrahera information från en textsträng.

## Så här gör du:

```Elm
import String

-- Ta bort alla förekomster av bokstaven "a" i en sträng
resultat = String.repeatedly (String.dropLeft 1 >> String.contains "a") "bana" -- "bn"
```
## Fördjupning:

### Historisk kontext:
Att ta bort tecken som matchar ett visst mönster är en vanlig funktion i många programmeringsspråk och har funnits sedan tidiga dagar av datoranvändande. I moderna språk, som Elm, är det ofta implementerat som en del av standardbiblioteket.

### Alternativ:
En annan metod för att ta bort tecken som matchar ett mönster är att använda en reguljär uttrycksökning, vilket ofta är mer kraftfullt men också mer komplicerat. Ett exempel på detta i Elm skulle vara att använda funktionen `Regex.replace` från `elm/regex`-paketet.

### Implementeringsdetaljer:
I Elm används funktionen `String.repeatedly` tillsammans med funktionerna `String.dropLeft` och `String.contains` för att ta bort tecken som matchar ett mönster. Först används `String.dropLeft` för att ta bort de första tecknen i strängen baserat på ett angivet antal, och sedan kontrolleras om de borttagna tecknen innehåller mönstret med hjälp av `String.contains`. Om mönstret finns i de borttagna tecknen, så tas de bort från strängen helt till dess att det inte finns fler förekomster av mönstret i strängen.

## Se även:
- [Dokumentation för `elm/core`-paketet](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Dokumentation för `elm/regex`-paketet](https://package.elm-lang.org/packages/elm/regex/latest/)