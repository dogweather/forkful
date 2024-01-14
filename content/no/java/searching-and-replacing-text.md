---
title:    "Java: Søke og erstatte tekst"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Vi har alle vært der - du har skrevet en lang kodeblokk og innser plutselig at du har brukt feil variabelnavn overalt. Eller kanskje du vil endre på en spesifikk streng i en tekstfil. Å erstatte tekst i koden din kan være en tidkrevende oppgave, men heldigvis er det en enkel løsning - søk og erstatt! Ved å bruke Java, kan du enkelt finne og endre teksten du trenger, og spare deg selv for mye frustrasjon og tid.

## Hvordan gjøre det

Først må du importere Java's `io` bibliotek, som vil tillate deg å lese og skrive til filer. Deretter kan du bruke `File` og `BufferedReader` klassene for å lese tekst fra en fil. For eksempel:

```Java
import java.io.*;

public class SearchAndReplace {

    public static void main(String[] args) {

        // Åpne filen med BufferedReader
        try (BufferedReader br = new BufferedReader(new FileReader("tekstfil.txt"))) {

            // Les linje for linje
            String line;
            while ((line = br.readLine()) != null) {

                // Søk etter teksten du vil endre og erstatt den med ønsket tekst
                line = line.replaceAll("feil variabelnavn", "riktig variabelnavn");

                // Skriv den endrede linjen til en ny fil
                try (PrintWriter pw = new PrintWriter(new FileWriter("ny_tekstfil.txt", true))) {
                    pw.println(line);
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Koden over vil lese fra filen "tekstfil.txt", finne alle forekomster av "feil variabelnavn" og erstatte dem med "riktig variabelnavn". Den nye teksten vil bli skrevet til en ny fil med navnet "ny_tekstfil.txt".

## Dypdykk

Java's `String` klasse har flere innebygde metoder som kan hjelpe deg med å finne og erstatte tekst. Noen eksempler inkluderer:

- `contains()` - returnerer en boolean verdi som indikerer om en spesifisert tekst finnes i en tekststreng.
- `replace()` - erstatter eksisterende tekst med ny tekst.
- `startsWith()` og `endsWith()` - sjekker om tekststrengen starter eller slutter med en spesifisert tekst.

Det finnes også ulike metoder for å utføre søk og erstatt i store tekstblokker, inkludert:

- `replaceFirst()` - erstatter den første forekomsten av en tekst.
- `replaceAll()` - erstatter alle forekomster av en tekst.

Det er viktig å merke seg at disse metodene er case-sensitive, så du må være nøye med å skrive inn teksten nøyaktig slik den dukker opp i koden din.

## Se også

- [Java String klasse](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java BufferedReader klasse](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Java PrintWriter klasse](https://docs.oracle.com/javase/8/docs/api/java/io/PrintWriter.html)