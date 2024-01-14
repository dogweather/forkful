---
title:    "Java: Sletting av tegn som samsvarer med et mønster"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor

I denne bloggposten skal vi gå gjennom hvordan man kan slette tegn som matcher et bestemt mønster i en Java-kode. Dette kan være nyttig når man jobber med tekstbehandling og ønsker å fjerne uønskede tegn eller upassende språk.

## Hvordan å gjøre det

Det er flere måter å slette karakterer som matcher et mønster i en Java-kode. Her skal vi se på to forskjellige metoder: Bruke regex (regular expressions) og bruke String-klassen. 

### Regex

For å slette tegn basert på et mønster, må vi bruke klassen Pattern og Matcher fra pakken java.util.regex. Her er et eksempel på hvordan dette kan gjøres:

```Java
import java.util.regex.*;

String tekst = "Dette er en tekst med uønskede !# tegn !#";
String mønster = "[!#]"; // Dette er mønsteret vi ønsker å matche

Pattern p = Pattern.compile(mønster);
Matcher m = p.matcher(tekst);
String nyTekst = m.replaceAll("");

System.out.println(nyTekst); // Vil printe ut "Dette er en tekst med uønskede tegn"
```

Her bruker vi metoden `replaceAll()` for å erstatte alle tegn som matcher mønsteret med et tomt streng. Dette vil fjerne alle forekomster av de uønskede tegnene i teksten.

### String-klassen

En annen måte å fjerne tegn som matcher et mønster er å bruke `replace()`-metoden fra String-klassen. Denne metoden tar inn to strenger og erstatter alle forekomster av den første strengen med den andre strengen. Her er et eksempel:

```Java
String tekst = "Dette er en tekst med uønskede !# tegn !#";
String mønster = "!#";

String nyTekst = tekst.replace(mønster, "");

System.out.println(nyTekst); // Vil printe ut "Dette er en tekst med uønskede tegn"
```

Her erstatter vi alle forekomster av mønsteret `!#` med en tom streng, noe som vil fjerne alle tegnene som matcher dette mønsteret.

## Deep Dive

Regex gir oss mer fleksibilitet når det kommer til å definere mønster for å matche tegn, mens `replace()`-metoden fra String-klassen er enklere å bruke for enklere mønstre. Det er også verdt å merke seg at disse metodene ikke endrer på den originale teksten, men returnerer en ny streng med de ønskede endringene.

## Se Også

- [Oracle's Regex-tutorial](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Java String-klassen](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-java.lang.CharSequence-java.lang.CharSequence-)