---
title:    "Haskell: Søking og bytting av tekst"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange av oss som programmerer i Haskell vet hvor viktig det er med effektiv og nøyaktig håndtering av tekst. Men noen ganger trenger vi å gjøre endringer i store mengder tekst på en effektiv måte. Dette er hvor søk og erstatting kommer inn i bildet. Ved å lære å søke og erstatte tekst, kan du spare tid og gjøre koden din mer strukturert og lesbar.

## Hvordan gjøre det

I Haskell finnes det flere måter å søke og erstatte tekst på. Den vanligste metoden er å bruke funksjonene `words` og `unwords`, som kan dele en streng inn i en liste av ord og tilbake til en enkelt streng igjen.

```Haskell
ordListe = words "Dette er en setning"
-- ["Dette", "er", "en", "setning"]

tekst = unwords ["Hello", "world"]
-- "Hello world"
```

En annen nyttig funksjon er `replace`, som gir deg muligheten til å erstatte deler av en streng med en ny streng. Denne funksjonen tar tre argumenter: teksten du vil søke etter, teksten du vil erstatte det med, og strengen du vil erstatte teksten i.

```Haskell
erstattTekst = replace "eple" "banan" "Jeg elsker epler"
-- "Jeg elsker bananer"
```

Du kan også bruke regex-uttrykk med `subRegex`-funksjonen for mer komplekse søk og erstattinger.

## Dykk dypere

Nå som du har lært å søke og erstatte tekst i Haskell, kan du interessere deg for å lære mer om regex og hvordan du kan bruke det til å søke og erstatte tekst mer avansert. Det finnes også mange nyttige biblioteker som kan hjelpe deg med å håndtere tekst i Haskell, som `text` og `bytestring`.

## Se også

- [Haskell tekstbehandling med regex](https://wiki.haskell.org/Regex)
- [Haskell "text" biblioteket](https://hackage.haskell.org/package/text)
- [Haskell "bytestring" biblioteket](https://hackage.haskell.org/package/bytestring)