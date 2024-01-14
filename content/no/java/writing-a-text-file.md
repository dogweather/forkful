---
title:    "Java: Skrive en tekstfil"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor

Skriver du ofte kode i Java, men lurer på hvorfor du skulle skrive en tekstfil? Det kan være flere grunner til å gjøre dette. En tekstfil er en enkel måte å lagre og lese data på. Dette gir mulighet for lagring av større mengder data som kan leses og bearbeides på en enkel måte.

## Hvordan

For å skrive en tekstfil i Java, må vi først lage en filobjekt og angi filnavn og bane. Dette gjøres ved hjelp av File-klassen. Deretter må vi lage en filskriver ved å bruke FileWriter-klassen. Vi kan deretter bruke write () -funksjonen for å skrive ønsket tekst til filen, og deretter lukke filen ved hjelp av close () -funksjonen. For å lese en tekstfil, må vi lage en filleser ved hjelp av FileReader-klassen, og deretter bruke read () -funksjonen for å lese data fra filen. Her er et eksempel på hvordan dette kan gjøres:

```Java
import java.io.*;

public class TextFileExample {

    public static void main(String[] args) {
        
        try {
            // Oppretter filobjekt og angir filnavn og bane
            File file = new File("tekstfil.txt");

            // Oppretter en filskriver
            FileWriter fileWriter = new FileWriter(file);

            // Skriver ønsket tekst til filen
            fileWriter.write("Dette er en tekst som vil bli lagret i tekstfilen.");

            // Lukker filen
            fileWriter.close();

            // Oppretter en filleser
            FileReader fileReader = new FileReader(file);

            // Leser data fra filen
            int character = fileReader.read();
            
            // Leser og skriver ut data en tegn av gangen
            while(character != -1) {
                System.out.print((char)character);
                character = fileReader.read();
            }

            // Lukker filen
            fileReader.close();
        }
        
        catch (IOException e) {
            System.out.println("En feil oppstod under lesing/skriving av filen.");
            e.printStackTrace();
        }
    }
}
```

Når du kjører programmet vil du se at teksten "Dette er en tekst som vil bli lagret i tekstfilen." skrives til filen, og deretter blir den samme teksten skrevet ut i konsollen.

## Dypdykk

Det er viktig å merke seg at når du leser fra en tekstfil, leser du tegn og ikke bokstaver. Dette betyr at for eksempel bokstaven "a" vil bli representert med et tall i UTF-8 kodingen, som igjen kan oversettes tilbake til bokstaven når vi skriver den ut. I tillegg finnes det også flere måter å lese og skrive til en tekstfil på, som for eksempel ved hjelp av BufferedReader og BufferedWriter-klassene.

## Se også

- [Java File-klassen](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java FileWriter-klassen](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)
- [Java FileReader-klassen](https://docs.oracle.com/javase/8/docs/api/java/io/FileReader.html)
- [Java BufferedReader-klassen](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Java BufferedWriter-klassen](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedWriter.html)