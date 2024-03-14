---
date: 2024-01-20 17:46:02.631360-07:00
description: "\xC5 trekke ut substrings betyr simpelthen \xE5 hente deler av en streng.\
  \ Programmerere gj\xF8r dette for \xE5 manipulere og behandle spesifikke data fra\
  \ st\xF8rre\u2026"
lastmod: '2024-03-13T22:44:40.655232-06:00'
model: gpt-4-1106-preview
summary: "\xC5 trekke ut substrings betyr simpelthen \xE5 hente deler av en streng.\
  \ Programmerere gj\xF8r dette for \xE5 manipulere og behandle spesifikke data fra\
  \ st\xF8rre\u2026"
title: Uthenting av delstrenger
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å trekke ut substrings betyr simpelthen å hente deler av en streng. Programmerere gjør dette for å manipulere og behandle spesifikke data fra større tekstblokker.

## Hvordan:
Her er et grunt dykk inn i substrings i Java:

```java
public class SubstringExample {
    public static void main(String[] args) {
        String tekst = "Hei, Norge!";
        String deltekst = tekst.substring(5, 10);
        System.out.println(deltekst);  // Skriver ut "Norge"
    }
}
```
Kjøring av programmet over vil gi følgende utskrift:

```
Norge
```

Hvis du trenger en substring fra et startindeks til slutten av strengen, dropp sluttpunktet:

```java
String deltekstUtenSlutt = tekst.substring(5);
System.out.println(deltekstUtenSlutt);  // Skriver ut "Norge!"
```

## Dypdykk:
Substring-operasjoner har vært en sentral del av string-håndtering siden programmeringens barndom. I Java, fra versjon 7 update 6, endret implementasjonen seg slik at hver substring fikk sin egen karakterarray, fremfor å dele array med originalen. Dette hjelper med hukommelsesstyring for store strenger, men kan også føre til økt hukommelsesbruk hvis substrings ikke håndteres forsiktig.

Noen alternatives metoder inkluderer `String.split()`, regulære uttrykk og `StringUtils` fra Apache Commons.

Historiske implementasjoner la også grunnlag for sikkerhetsproblemer hvor sensitive data kunne lekke gjennom substrings, noe som nødvendiggjorde forsiktig håndtering og grundige sikkerhetsvurderinger.

## Se Også:
- [Oracle Java Docs for String](https://docs.oracle.com/en/java/javase/)
- [Apache Commons StringUtils](https://commons.apache.org/proper/commons-lang/)
- [Regular Expressions in Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Effective Java by Joshua Bloch](https://www.pearson.com/us/higher-education/program/Bloch-Effective-Java-3rd-Edition/PGM334842.html) for best practices.
