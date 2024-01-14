---
title:                "Java: Utskrift av feilsøkingsutdata"
programming_language: "Java"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor?

Før du begynner å skrive kode, tenk på hvorfor du ønsker å skrive den. Hvorfor skulle du bry deg om å inkludere utskrift av feil i koden din? Det kan virke unødvendig, men det er faktisk en viktig del av utviklingsprosessen. Ved å inkludere utskrift av feil, kan du enkelt diagnostisere og løse problemer i koden din.

## Slik gjør du det

Det er enkelt å inkludere utskrift av feil i Java-koden din. Alt du trenger å gjøre er å bruke metoden `System.out.println()` for å skrive ut en beskjed til konsollen. La oss se på et eksempel:

```Java
int x = 5;
int y = 0;
int result = x / y;
System.out.println("Resultat: " + result);
```

I dette tilfellet vil det oppstå en feil siden vi prøver å dele med 0. Ved å inkludere utskrift av `result`-variabelen, kan vi se at feilene kommer fra der og enkelt rette dem.

## Dypdykk

Når det gjelder utskrift av feil, er det forskjellige metoder du kan bruke for å få mer spesifikk informasjon. På denne måten kan du finne ut mer om hvor og når feilen skjer. Her er noen nyttige metoder:

- `System.out.println()` - for å skrive ut generell informasjon
- `System.err.println()` - for å skrive ut feilmeldinger
- `System.out.printf()` - for å printe formatering
- `System.out.format()` - for å printe formatering

Ved bruk av disse metodene og å inkludere variabler i utskriften din, kan du få en dypere forståelse av feilene i koden din.

## Se også

- [How to Use Debugging Techniques in Java](https://www.informit.com/articles/article.aspx?p=1946058)
- [Debugging in Java](https://www.baeldung.com/java-debugging)
- [Debug Your Java Code With Eclipse](https://www.codejava.net/ides/eclipse/how-to-debug-java-code-in-eclipse)