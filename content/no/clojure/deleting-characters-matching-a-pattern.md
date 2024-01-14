---
title:    "Clojure: Sletting av tegn som matcher et mønster"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor

Når man jobber med programmering, er det ofte nødvendig å manipulere tekststrenger for å få ønsket resultat. En nyttig funksjon i Clojure for dette er "remove", som lar oss slette deler av en tekststreng basert på et gitt mønster. Dette kan være nyttig for å rydde opp i data eller for å filtrere ut uønskede tegn.

## Hvordan

For å slette tegn som matcher et gitt mønster, kan vi bruke "remove" funksjonen på følgende måte:

```Clojure
(remove #"[a-z]" "Hello123") ; => "123"
```

Her vil funksjonen fjerne alle små bokstaver fra tekststrengen "Hello123" og returnere "123". Dette er fordi #-prefikset forteller Clojure at vi bruker et regulært uttrykk for å definere mønsteret vi vil slette.

Vi kan også bruke "remove" på en liste med strenger, for eksempel:

```Clojure
(remove #"[0-9]" ["Hello123" "987World"]) ; => ("Hello" "World")
```

Dette vil returnere en liste med strenger der alle tall er slettet fra hver streng.

## Dypdykk

Når vi bruker "remove" funksjonen, endres ikke den opprinnelige tekststrengen eller listen vi bruker som input. Funksjonen returnerer en ny kopi av teksten med de ønskede tegnene slettet. Dette er viktig å huske på når vi jobber med store mengder data, for å unngå utilsiktede endringer.

Vi kan også kombinere flere mønstre ved å bruke "or" operator i det regulære uttrykket, for eksempel:

```Clojure
(remove #"[a-z]|[A-Z]" "Hello123World") ; => "123"
```

Dette vil fjerne både små og store bokstaver fra tekststrengen "Hello123World".

## Se også

- [Clojure Docs for Remove](https://clojuredocs.org/clojure.core/remove)
- [Regulære uttrykk i Clojure](https://clojure.org/guides/learn/regular_expressions)