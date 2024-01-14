---
title:                "PHP: Å finne lengden til en streng"
simple_title:         "Å finne lengden til en streng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

"Når vi jobber med å lage nettsider og applikasjoner, vil vi ofte måtte håndtere tekst og informasjon som brukerene skriver inn. En viktig del av dette er å kunne finne lengden på en tekststreng. Dette er nyttig i mange situasjoner, som for eksempel når vi skal validere inndata eller når vi trenger å telle antall tegn i en tekst.

## Hvorfor

Å finne lengden på en tekststreng er en grunnleggende og vanlig oppgave i PHP-programmering. Dette er viktig for å kunne formatere og behandle tekst på riktig måte. Det kan også være nyttig når vi skal utføre operasjoner som å telle antall ord eller dele opp en tekststreng i mindre deler.

## Slik gjør du det

Å finne lengden på en tekststreng i PHP er enkelt og kan gjøres ved hjelp av en innebygd funksjon kalt `strlen()`. Denne funksjonen tar inn en tekststreng som parameter og returnerer antall tegn i strengen. La oss se på et eksempel:

```PHP
$name = "Ola Nordmann";
$length = strlen($name);

echo "Navnet ditt er $length tegn langt.";
```

I dette eksemplet lagrer vi en tekststreng i variabelen `$name` og bruker deretter `strlen()`-funksjonen til å finne lengden på denne strengen. Deretter bruker vi `echo` for å skrive ut en melding med lengden av tekststrengen.

Output:
```
Navnet ditt er 12 tegn langt.
```

Vi kan også bruke `strlen()`-funksjonen til å finne lengden på en tekststreng på en mer dynamisk måte, for eksempel ved å kombinere den med en `for`-løkke:

```PHP
$word = "banana";
$length = strlen($word);

for ($i = 0; $i < $length; $i++) {
    echo "Dette ordet har $length bokstaver." . PHP_EOL;
}
```

Output:
```
Dette ordet har 6 bokstaver.
Dette ordet har 6 bokstaver.
Dette ordet har 6 bokstaver.
Dette ordet har 6 bokstaver.
Dette ordet har 6 bokstaver.
Dette ordet har 6 bokstaver.
```

## Dypdykk

`strlen()`-funksjonen i PHP er basert på Unicode-tegnsettet, noe som betyr at den kan håndtere alle typer tegn, inkludert multibyte-tegn. Dette gjør den mer pålitelig og presis enn å bruke `mb_strlen()`-funksjonen. Imidlertid er det verdt å merke seg at `strlen()` ikke teller antall ord i en tekststreng, men antall tegn. Dette er viktig å være klar over når du bruker denne funksjonen.

## Se også

- [PHP strlen() Function](https://www.php.net/manual/en/function.strlen.php)
- [PHP mb_strlen() Function](https://www.php.net/manual/en/function.mb-strlen.php)