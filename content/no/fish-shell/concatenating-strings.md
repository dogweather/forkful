---
title:                "Fish Shell: Sammenslåing av tekststrenger"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sette sammen strenger, eller konkatenere, er en vanlig oppgave i programmering. Det lar deg kombinere flere strenger til en enkelt streng, som kan være nyttig for å formatere utdata eller bygge URL-er. I Fish Shell kan vi bruke kommandoen `string` for å konkatenere strenger.

## Slik gjør du det

For å konkatenere strenger i Fish Shell, kan du bruke følgende syntaks:

```fish
string concat "tekst1" "tekst2"
```

Dette vil gi utdata `tekst1tekst2`. Merk at vi må bruke anførselstegn rundt hver streng vi ønsker å konkatenere.

Du kan også kombinere variabler med strenger ved å bruke `$` tegnet og plassere variablene i en `string` kommando. For eksempel:

```fish
set navn "John Doe"
string concat "Hei $navn, velkommen til min blogg!"
```

Dette vil gi utdata `Hei John Doe, velkommen til min blogg!`, der `navn` variabelen blir erstattet med verdien som er lagret i den.

Vi kan også konkatenere flere strenger samme i en `string` kommando ved å legge til flere argumenter, som i følgende eksempel:

```fish
string concat "Dette" "er" "en" "test."
```

Dette vil gi utdata `Dette er en test.`

## Dypdykk

I tillegg til å bruke `string` kommandoen, kan man også bruke `echo` kommandoen til å konkatenere strenger. Dette gjøres ved å bruke `-s` flagget, som står for "separator". Så kan man spesifisere hvilket tegn som skal brukes som separator. For eksempel:

```fish
echo -s " " "Dette" "er" "en" "test."
```

Dette vil gi utdata `Dette er en test.`

En annen måte å konkatenere strenger på er ved hjelp av `set` kommandoen og `string replace` kommandoen. Dette kan være nyttig hvis du vil endre en del av en streng før du konkatenere den. For eksempel:

```fish
set tall 123
set tekst "Dette er et tall: 456"
string concat "Dette er et tall: " (string replace "123" $tall $tekst)
```

Dette vil gi utdata `Dette er et tall: 456`, der tallet 123 blir erstattet med verdien lagret i `tall` variabelen.

## Se også

- [Offisiell Fish Shell dokumentasjon for string kommando](https://fishshell.com/docs/current/commands.html#string)
- [Teknikker for å jobbe med tekststrenger i Fish Shell](https://blog.martinbuberl.com/working-with-strings-in-fish-shell/)
- [Praktiske eksempler på å bruke `string` kommandoen](https://medium.com/@rajatm1104/concatenate-a-string-in-fish-shell-8d905d197b49)