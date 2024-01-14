---
title:                "Java: Arbeide med csv"
simple_title:         "Arbeide med csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

CSV-filer er en vanlig filtype som brukes til å lagre og organisere data. Som en utvikler, kan du støte på CSV-filer regelmessig, enten du må importere og behandle dataene eller eksportere dem til et annet programvareverktøy. Derfor er det viktig å ha kunnskap om hvordan man arbeider med CSV-filer i Java.

## Hvordan

Kort fortalt, CSV-filer er enkle tekstfiler som består av komma-separerte verdier. Her vist et eksempel på en CSV-fil:

```Java
Navn,Alder,Yrke
Per,35,Kokk
Kari,28,Lærer
Ole,42,Advokat
```

For å lese og bearbeide en CSV-fil i Java, kan du bruke klassen `Scanner`. Her er et eksempel på hvordan du kan bruke `Scanner` til å lese og skrive ut dataene i CSV-filen ovenfor:

```Java
try {
    File csvFile = new File("minfil.csv");
    Scanner scanner = new Scanner(csvFile);

    // Hopp over første linje som inneholder overskrifter
    scanner.nextLine();

    // Løkke gjennom radene i filen
    while (scanner.hasNextLine()) {
        // Leser neste linje og deler den opp etter komma
        String[] data = scanner.nextLine().split(",");

        // Skriver ut dataene
        System.out.println(data[0] + " er " + data[1] + " år gammel og jobber som " + data[2]);
    }

    // Lukker scanneren
    scanner.close();
} catch (FileNotFoundException e) {
    e.printStackTrace();
}
```

Output:

```
Per er 35 år gammel og jobber som Kokk
Kari er 28 år gammel og jobber som Lærer
Ole er 42 år gammel og jobber som Advokat
```

## Dypdykk

Når du arbeider med CSV-filer, er det viktig å huske på at dataene kan være i ulik format og kvalitet. Dette kan føre til utfordringer når du skal lese og behandle filen. Derfor er det viktig å være grundig når du utvikler koden din for håndtering av CSV-filer.

En annen ting du bør være oppmerksom på, er håndtering av spesialtegn. Hvis en verdi i CSV-filen inneholder et komma eller et linjeskift, vil dette kunne føre til at dataene blir lest inn feil. Det er derfor viktig å håndtere og behandle disse spesialtegnene riktig, for å sikre nøyaktig lesing og behandling av dataene.

## Se også

- [Java Scanner-klassen](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- [CSV-filer på Stack Overflow](https://stackoverflow.com/questions/tagged/csv)
- [OpenCSV-biblioteket for Java](http://opencsv.sourceforge.net/)