---
title:    "Java: Uthenting av substringer"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Hvorfor

Å trekke ut substringer er en vanlig oppgave innenfor Java-programmering, og kan være nyttig for å manipulere tekststrenger. Det lar oss hente ut en del av en streng basert på plassering, lengde eller et spesifikt mønster.

# Slik gjør du det

For å trekke ut en substring i Java, kan vi bruke metoden `substring()` på en String-variabel. Denne metoden tar inn to argumenter: startindeksen og sluttindeksen for den ønskede delen av strengen.

```Java
String navn = "Jørgen Andersen";
String etternavn = navn.substring(8, 16);
System.out.println(etternavn);
```

Dette vil skrive ut "Andersen" i terminalen. I dette tilfellet har vi angitt startindeksen som 8 for å hoppe over mellomnavnet "Ørgen" og sluttindeksen som 16 for å inkludere bokstaven "n".

Vi kan også bruke metoden `indexOf()` for å finne plasseringen til et spesifikt mønster i en streng og deretter bruke `substring()` for å hente ut en del av strengen basert på dette mønsteret.

```Java
String tekst = "Hei alle sammen!";
int indeks = tekst.indexOf("alle");
String delstreng = tekst.substring(indeks);
System.out.println(delstreng);
```

Her vil `indexOf()` returnere indeksen til bokstaven "a" i ordet "alle", som er 4. Vi bruker deretter denne indeksen i `substring()` for å hente ut resten av strengen etter dette mønsteret. Dette vil skrive ut "alle sammen!" i terminalen.

# Dykk ned i detaljene

Når vi bruker metoden `substring()`, må vi være oppmerksomme på at startindeksen må være mindre enn sluttindeksen. Hvis de to indeksene er like, vil metoden returnere en tom streng. Hvis sluttindeksen er større enn lengden på den opprinnelige strengen, vil metoden returnere en delstreng som slutter på slutten av den opprinnelige strengen.

Vi kan også bruke `substring()` for å erstatte deler av en streng med en annen streng ved hjelp av metoden `replace()`.

```Java
String setning = "Jeg spiser frokost.";
String nySetning = setning.substring(4, 11).replace("spiser", "elsker") + " lunsj.";
System.out.println(nySetning);
```

Her vil `substring()` returnere "spiser", som vi deretter erstatter med "elsker". Dette vil gi oss "Jeg elsker frokost. Jeg spiser lunsj." som utskrift i terminalen.

# Se også

- Java String API: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
- Tutorialspoint: https://www.tutorialspoint.com/java/lang/string_substring.htm
- W3Schools: https://www.w3schools.com/java/java_strings_sub.asp