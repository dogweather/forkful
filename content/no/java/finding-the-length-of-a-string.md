---
title:                "Å finne lengden av en streng"
html_title:           "Java: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Enten du er en erfaren Java-programmerer eller nybegynner, er det viktig å forstå hvordan å finne lengden til en streng. Dette er en grunnleggende funksjon som brukes i mange programmer og kan hjelpe deg å håndtere data mer effektivt.

## Hvordan du gjør det

For å finne lengden til en streng i Java, kan du bruke metoden `length()`. Her er et enkelt eksempel som viser hvordan du bruker denne metoden:

```Java
String navn = "Ole";
int lengde = navn.length();
System.out.println(lengde);
```

Dette vil gi deg outputen `3`, siden strengen "Ole" består av tre bokstaver. Hvis du ønsker å finne lengden av en tekst som brukeren skriver inn, kan du bruke en skanner og deretter kalle `length()`-metoden på den skannede strengen.

```Java
Scanner in = new Scanner(System.in);
System.out.print("Skriv inn en setning: ");
String setning = in.nextLine();
int lengde = setning.length();
System.out.println(lengde);
```

For eksempel, hvis brukeren skriver inn "Jeg elsker å programmere!", vil outputen bli `22`.

## Deep Dive

Den `length()`-metoden vi brukte i eksemplene ovenfor, returnerer antall tegn i en streng. Men visste du at den faktisk teller antall Unicode-tegn, ikke bare bokstaver?

En Unicode-karakter er et standardisert tall som representerer et skrifttegn eller et symbol for alle språk. Dette betyr at hvilken som helst streng, uansett språk, vil ha en korrekt lengde. For eksempel, hvis du bruker `length()`-metoden på en streng som inneholder kinesiske tegn, vil den returnere det riktige antall tegn.

En annen viktig ting å merke seg når du bruker `length()`-metoden, er at den teller med alle mellomrom og spesialtegn. Dette betyr at en streng som kun inneholder ett mellomrom vil ha en lengde på 1.

## See Also

* [Javas offisielle dokumentasjon for `length()`-metoden](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)(engelsk) 
* [Tutorial for å jobbe med strenger i Java (på norsk)](https://www.ntnu.no/wiki/pages/viewpage.action?pageId=2953804)