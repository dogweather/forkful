---
title:    "Java: Utdrag av substringer"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å ekstrahere substrings, eller delstrenger, fra en tekststreng er en vanlig oppgave i Java-programmering. Dette kan være nyttig for å manipulere tekst, analysere data eller lage mer dynamiske programmer. Det er en enkel og effektiv måte å få tak i en bestemt del av en tekststreng på, og kan bidra til å gjøre koden din mer lesbar og enklere å vedlikeholde.

## Hvordan

For å ekstrahere en substring i Java, bruker du metoden `substring()` med to parametere. Den første parameteren er startindeksen i tekststrengen, mens den andre parameteren er sluttposisjonen. For eksempel, hvis du vil få tak i bare de første fire bokstavene i et ord, kan du bruke følgende kode:

```
Java String ord = "programmering";
String delord = ord.substring(0,4);
System.out.println(delord);
```

Dette vil gi følgende utskrift:

```
prog
```

Du kan også bruke negative indekser for å telle baklengs fra slutten av tekststrengen. For eksempel, hvis du vil få tak i de to siste bokstavene i ordet "programmering", kan du bruke følgende kode:

```
Java String ord = "programmering";
String delord = ord.substring(ord.length()-2, ord.length());
System.out.println(delord);
```

Dette vil gi følgende utskrift:

```
ng
```

## Dypdykk

Det er viktig å huske at indeksering i Java starter på 0, så den første bokstaven i en tekststreng har alltid indeksen 0. Det er også viktig å sjekke at indeksene du bruker i `substring()`-metoden, ikke overskrider lengden på tekststrengen for å unngå en `IndexOutOfBoundsException`.

En annen nyttig metode for å ekstrahere substrings er `split()`-metoden, som deler en tekststreng opp i et array med delstrenger basert på et gitt tegn eller mønster. For eksempel, hvis du vil dele opp en tekststreng basert på mellomrom, kan du bruke følgende kode:

```
Java String setning = "Dette er en setning";
String[] ord = setning.split(" ");
for(String delord : ord){
  System.out.println(delord);
}
```

Dette vil gi følgende utskrift:

```
Dette
er
en
setning
```

## Se Også

* [Java String Dokumentasjon](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html)
* [W3Schools - Java String Handling](https://www.w3schools.com/java/java_strings.asp)
* [GeeksforGeeks - Java String substring()](https://www.geeksforgeeks.org/java-string-substring-method-example/)
* [Baeldung - Split Strings in Java](https://www.baeldung.com/java-string-split)