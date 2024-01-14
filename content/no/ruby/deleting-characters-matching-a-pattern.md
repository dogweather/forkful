---
title:                "Ruby: Fjerning av tegn som matcher et mønster"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være nødvendig å slette visse tegn fra en tekststreng eller et helt dokument. Dette kan være for å rydde opp i unødvendig informasjon, formatere data eller forandre formål. I denne bloggposten vil vi se på hvordan du kan slette tegn som matcher et bestemt mønster i Ruby, og hvorfor dette kan være nyttig.

## Hvordan

Det finnes flere måter å slette tegn som matcher et mønster i Ruby. En måte er å bruke metoden `gsub` som står for "global substitute". Denne metoden erstatter alle forekomster av et mønster med en tom streng. Her er et eksempel med tekststrengen "Hei alle sammen!":

```Ruby
"Hei alle sammen!".gsub(/[a-z]/, "")
```

Dette vil gi følgende output:

```Ruby
"Hei!"
```

Som du kan se, blir alle små bokstaver slettet fra teksten.

En annen måte å slette tegn på er å bruke metoden `delete`, som også tar inn et mønster som argument. Dette vil slette alle tegn som matcher mønsteret. Her er et eksempel med tekststrengen "12345abcdefgh":

```Ruby
"12345abcdefgh".delete("1-5")
```

Dette vil gi følgende output:

```Ruby
"abcdefgh"
```

Som du kan se, blir alle tallene fra 1 til 5 slettet fra teksten.

## Dypdykk

Det er viktig å være klar over at metoden `gsub` tar i bruk regulære uttrykk (regex) for å matche et mønster i teksten. Dette gjør det svært fleksibelt og kraftig, men kan også være utfordrende for nybegynnere. Du kan lese mer om regulære uttrykk og hvordan de fungerer i Ruby [her](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm).

I tillegg kan du også kombinere flere metoder for å slette spesifikke tegn fra teksten. For eksempel kan du først bruke `gsub` til å slette alle tall, og deretter bruke `delete` for å fjerne alle vokaler. Det er ingen grenser for hvilke kombinasjoner du kan eksperimentere med for å få ønsket resultat.

## Se også

- [Ruby dokumentasjon for metoden `gsub`](https://ruby-doc.org/core-3.0.0/String.html#method-i-gsub)
- [Ruby dokumentasjon for metoden `delete`](https://ruby-doc.org/core-3.0.0/String.html#method-i-delete)
- [Tutorial om regulære uttrykk i Ruby](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)