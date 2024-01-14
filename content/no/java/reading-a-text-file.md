---
title:    "Java: Lese en tekstfil"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Hvorfor:

Hvis du er en Java-programmerer som ønsker å lære hvordan du kan lese og håndtere tekstfiler, er dette blogginnlegget for deg! Å kunne lese tekstfiler er en viktig ferdighet som kan være nyttig for å håndtere ulike typer data i programmene dine.

# Hvordan:

For å kunne lese en tekstfil i Java, må du først åpne filen ved å bruke File og FileReader klassene. Deretter kan du bruke BufferedReader klassen for å lese filen linje for linje. Her er et eksempel på hvordan du kan lese en tekstfil med navnet "tekstfil.txt" og skrive ut innholdet på konsollen:

```Java
public class LeseTekstfil {
    public static void main(String[] args) {
        try {
            File fil = new File("tekstfil.txt");
            FileReader fr = new FileReader(fil);
            BufferedReader br = new BufferedReader(fr);

            // Leser og skriver ut hver linje i filen
            String linje = br.readLine();
            while (linje != null) {
                System.out.println(linje);
                linje = br.readLine();
            }
            br.close();
        } catch (IOException e) {
            System.out.println("Kunne ikke lese fil: " + e);
        }
    }
}
```

Output av dette programmet vil være:

Dette er en tekstfil.
Den inneholder litt informasjon.
Denne linjen vil også bli skrevet ut.

# Deep Dive:

Nå som du vet hvordan du kan lese en tekstfil og skrive ut innholdet, kan det være nyttig å lære litt mer om hvordan du kan håndtere forskjellige typer data i en tekstfil. Java har en rekke klasser og metoder som kan hjelpe deg med dette.

For eksempel, hvis du ønsker å kunne lese andre typer data enn bare tekst, kan du bruke Scanner klassen i stedet for BufferedReader. Du kan også bruke FileWriter og PrintWriter for å kunne skrive til tekstfiler. I tillegg kan du bruke StringTokenizer klassen for å kunne arbeide med datadelte tekster.

Det er også mulig å lese og skrive til tekstfiler som er lagret på fjerntliggende datamaskiner ved å bruke URL klassen. Og hvis du ønsker å lese og skrive til databasefiler, kan du bruke JDBC API som gir en grensesnitt for å håndtere databasebaserte data.

Å kunne lese og håndtere tekstfiler er en viktig ferdighet for enhver Java-programmerer, og det finnes et bredt spekter av muligheter og verktøy for å gjøre dette i Java.

# Se også:

For mer informasjon om hvordan du kan håndtere tekstfiler i Java, kan du sjekke ut disse nyttige ressursene:

- [Oracle Java Documentation](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Java FileReader klasse](https://docs.oracle.com/javase/7/docs/api/java/io/FileReader.html)
- [Java BufferedReader klasse](https://docs.oracle.com/javase/7/docs/api/java/io/BufferedReader.html)
- [Java StringTokenizer klasse](https://docs.oracle.com/javase/7/docs/api/java/util/StringTokenizer.html)
- [Java File klassen](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java FileWriter klasse](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)
- [Java PrintWriter klasse](https://docs.oracle.com/javase/7/docs/api/java/io/PrintWriter.html)
- [Java Scanner klasse](https://docs.oracle.com/javase/7/docs/api/java/util/Scanner.html)
- [Java URL klasse](https://docs.oracle.com/javase/7/docs/api/java/net/URL.html)
- [Java JDBC API](https://docs.oracle.com/javase/tutorial/jdbc/overview/index.html)