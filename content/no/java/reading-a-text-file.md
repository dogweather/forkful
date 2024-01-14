---
title:    "Java: Leser en tekstfil"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Lurer du på hvorfor du burde lese en tekstfil i Java? Tekstfiler er en av de mest grunnleggende og nyttige måtene å lagre og organisere data på i programutvikling. De lar deg enkelt lese og behandle store mengder tekstbasert informasjon. Så hvis du ønsker å lære mer om hvordan du kan håndtere og manipulere data i programmene dine, er det viktig å ha en forståelse av hvordan du leser en tekstfil i Java.

# Slik gjør du det

Det finnes flere måter å lese en tekstfil i Java på, men i dette blogginnlegget vil vi fokusere på den vanligste metoden - ved hjelp av FileReader og BufferedReader. Her er et eksempel på kode som viser hvordan du kan lese hver linje i en tekstfil og skrive ut den til konsollen:

```Java
try {
    // Opprett FileReader og BufferedReader objekter
    FileReader fileReader = new FileReader("minTekstfil.txt");
    BufferedReader bufferedReader = new BufferedReader(fileReader);

    String line;

    // Les hver linje i tekstfilen og skriv den ut
    while ((line = bufferedReader.readLine()) != null) {
        System.out.println(line);
    }

    // Husk å lukke buffered readeren
    bufferedReader.close();
} catch (IOException e) {
    // Hvis det oppstår en feil, skriv ut feilmeldingen
    System.out.println("Det oppstod en feil ved lesing av tekstfilen: " + e.getMessage());
}
```

Koden vil åpne en fil som heter "minTekstfil.txt" og lese hver linje i filen ved hjelp av BufferedReader-objektet. Deretter skriver den ut hver linje til konsollen. Husk å lukke buffered readeren når du er ferdig med å lese filen for å unngå eventuelle lekkasjer.

Her er et eksempel på hvordan outputen kan se ut hvis tekstfilen inneholder tre linjer med tekst:

```shell
Dette er linje 1
Dette er linje 2
Dette er linje 3
```

# Dykk ned i detaljene

Nå som du vet hvordan du kan lese en tekstfil i Java, la oss dykke litt dypere inn i hva som faktisk skjer bak kulissene. Når du oppretter et FileReader-objekt, åpner det den angitte filen for å lese. Den bruker da et Buffer for å lese og lagre dataene fra filen. Dette gjør lesingen mer effektiv ved å minimere antall ganger den faktiske filen må leses fra lagringsenheten.

BufferedReader-objektet tilbyr også flere nyttige metoder for å lese og behandle data fra filen, som for eksempel å lese en bestemt mengde tegn eller skrive dataene til en annen fil.

# Se også

- [Java FileReader dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/io/FileReader.html)
- [Java BufferedReader dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Java File I/O tutorial](https://www.baeldung.com/java-file-io)