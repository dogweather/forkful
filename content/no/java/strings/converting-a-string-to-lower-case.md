---
date: 2024-01-20 17:38:44.996398-07:00
description: "\xC5 konvertere en streng til sm\xE5 bokstaver betyr \xE5 endre alle\
  \ tegn i strengen til deres nedre kasse ekvivalent. Programmerere gj\xF8r dette\
  \ for \xE5 standardisere\u2026"
lastmod: '2024-03-11T00:14:14.193190-06:00'
model: gpt-4-1106-preview
summary: "\xC5 konvertere en streng til sm\xE5 bokstaver betyr \xE5 endre alle tegn\
  \ i strengen til deres nedre kasse ekvivalent. Programmerere gj\xF8r dette for \xE5\
  \ standardisere\u2026"
title: "Konvertere en streng til sm\xE5 bokstaver"
---

{{< edit_this_page >}}

## What & Why?
Å konvertere en streng til små bokstaver betyr å endre alle tegn i strengen til deres nedre kasse ekvivalent. Programmerere gjør dette for å standardisere tekstdata, for eksempel for å sammenligne strenger uten å bry seg om bokstavstørrelse.

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
