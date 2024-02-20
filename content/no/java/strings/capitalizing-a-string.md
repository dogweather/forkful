---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:46.940187-07:00
description: "Det \xE5 sette stor bokstav i en streng inneb\xE6rer \xE5 endre den\
  \ f\xF8rste bokstaven i hvert ord i strengen til stor bokstav, mens man s\xF8rger\
  \ for at resten forblir\u2026"
lastmod: 2024-02-19 22:04:59.893912
model: gpt-4-0125-preview
summary: "Det \xE5 sette stor bokstav i en streng inneb\xE6rer \xE5 endre den f\xF8\
  rste bokstaven i hvert ord i strengen til stor bokstav, mens man s\xF8rger for at\
  \ resten forblir\u2026"
title: Sette stor bokstav i en streng
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Det å sette stor bokstav i en streng innebærer å endre den første bokstaven i hvert ord i strengen til stor bokstav, mens man sørger for at resten forblir små bokstaver. Denne vanlige manipulasjonen av strenger er nyttig for formatering av tekst i applikasjoner, slik som forberedelse av brukernavn eller titler for visning i henhold til konvensjon eller grammatikal korrekthet.

## Hvordan:
Java sitt standardbibliotek tilbyr ikke en direkte metode for å sette stor bokstav i hele strenger i ett steg, men du kan oppnå dette med en kombinasjon av innebygde metoder. For mer sofistikerte behov tilbyr tredjepartsbiblioteker som Apache Commons Lang enkle løsninger.

### Bruke Javas innebygde metoder
For å sette stor bokstav i en streng uten eksterne biblioteker, kan du dele strengen opp i ord, sette stor bokstav i den første bokstaven av hvert ord, og deretter sette dem sammen igjen. Her er en enkel tilnærming:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String tekst = "hallo, verden!";
        String kapitalisertTekst = kapitaliserOrd(tekst);
        System.out.println(kapitalisertTekst); // Skriver ut: "Hallo, Verden!"
    }

    public static String kapitaliserOrd(String str) {
        char[] tegn = str.toLowerCase().toCharArray();
        boolean funnet = false;
        for (int i = 0; i < tegn.length; i++) {
            if (!funnet && Character.isLetter(tegn[i])) {
                tegn[i] = Character.toUpperCase(tegn[i]);
                funnet = true;
            } else if (Character.isWhitespace(tegn[i]) || tegn[i]=='.' || tegn[i]=='\'') { 
                funnet = false;
            }
        }
        return String.valueOf(tegn);
    }
}
```

Dette kodeutdraget konverterer hele strengen til små bokstaver, og så itererer gjennom hver karakter, og setter stor bokstav i den første bokstaven i hvert ord. Det tar hensyn til mellomrom, punktum, og apostrofer som ordseparatorer.

### Bruke Apache Commons Lang

Apache Commons Lang-biblioteket tilbyr en mer elegant løsning med metoden `WordUtils.capitalizeFully()`, som håndterer ulike kanttilfeller og skilletegn for deg:

```java
// Legg til avhengighet: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String tekst = "hallo, verden!";
        String kapitalisertTekst = WordUtils.capitalizeFully(tekst);
        System.out.println(kapitalisertTekst); // Skriver ut: "Hallo, Verden!"
    }
}
```

For å bruke denne metoden må du legge til Apache Commons Lang-biblioteket i prosjektet ditt. Denne metoden i biblioteket setter ikke bare stor bokstav i den første bokstaven i hvert ord, men konverterer også resten av bokstavene i hvert ord til små bokstaver, noe som sikrer et konsistent mønster med stor bokstav gjennom hele strengen.
