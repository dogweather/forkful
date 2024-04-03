---
date: 2024-01-20 17:38:44.996398-07:00
description: "How to: Bruk `toLowerCase()` metoden for \xE5 konvertere en streng til\
  \ sm\xE5 bokstaver."
lastmod: '2024-03-13T22:44:40.653365-06:00'
model: gpt-4-1106-preview
summary: "Bruk `toLowerCase()` metoden for \xE5 konvertere en streng til sm\xE5 bokstaver."
title: "Konvertere en streng til sm\xE5 bokstaver"
weight: 4
---

## How to:
Bruk `toLowerCase()` metoden for å konvertere en streng til små bokstaver:

```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String original = "Hei Verden!";
        String lowerCase = original.toLowerCase();
        System.out.println(lowerCase);
    }
}
```

Kjører du koden, får du dette resultatet:

```
hei verden!
```

## Deep Dive
Å konvertere tekst til små bokstaver har røtter tilbake til tidlige datadager når det var klare forskjeller på små og store bokstaver i filnavn og kommandoer. I dag hjelper det i situasjoner som tekstbehandling og datasøk for å oppnå case-insensitive matching.

Alternativer til `toLowerCase()` inkluderer `toUpperCase()` for å gjøre alle bokstaver store og `Locale`-sensitive operasjoner, hvor nedre kasse konvertering respekterer språkregler.

Java `String` klassen benytter Unicode standarden for `toLowerCase()` som tar hensyn til språkspesifikke regler. I noen tilfeller kan dette gi overraskende resultater, siden nedre kasse versjoner av bokstaver ikke alltid er direkte og entydig mappet fra sine store bokstavpar.

## See Also
Mer informasjon og detaljer om metoden `toLowerCase()` og relaterte emner:
- Java dokumentasjon for `String.toLowerCase()`: https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#toLowerCase()
- Internasjonalisering og `Locale` klassen: https://docs.oracle.com/javase/tutorial/i18n/locale/
- Unicode Consortium om case mapping: http://www.unicode.org/reports/tr21/tr21-5.html
