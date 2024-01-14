---
title:                "Java: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

JSON (JavaScript Object Notation) er et populært format for å utveksle data på tvers av forskjellige plattformer og programmeringsspråk. Det er spesielt nyttig for webapplikasjoner som må håndtere store mengder data, som for eksempel sosiale medier og e-handelssider. Ved å lære å jobbe med JSON, kan du øke dine programmeringsferdigheter og bli en mer allsidig utvikler.

## Slik gjør du det

Å jobbe med JSON i Java er relativt enkelt takket være Java API-er som støtter JSON-formatet. Først må du importere pakken "org.json" i Java-filen din. Deretter kan du bruke JSONObject- og JSONArray-klassene til å lese og skrive JSON-data.

La oss si at du har en JSON-fil som inneholder informasjon om en bokhandel med navn, adresse og en liste over bøker. For å lese denne filen og skrive ut informasjonen, kan du bruke følgende kode:

```Java
import org.json.*;
import java.io.*;

public class JsonExample {

    public static void main(String[] args) throws IOException, JSONException {

        // Les inn JSON-filen
        FileReader reader = new FileReader("bookstore.json");

        // Lag en JSONObject for å lese dataene
        JSONObject bookstore = new JSONObject(reader);

        // Hent navn og adresse fra JSONObject og skriv dem ut
        System.out.println("Bokhandelnavn: " + bookstore.getString("name"));
        System.out.println("Adresse: " + bookstore.getString("address"));

        // Hent listen over bøker fra JSONObject og skriv ut tittel og forfatter for hver bok
        JSONArray books = bookstore.getJSONArray("books");
        for(int i = 0; i < books.length(); i++) {
            JSONObject book = books.getJSONObject(i);
            System.out.println("Tittel: " + book.getString("title"));
            System.out.println("Forfatter: " + book.getString("author"));
        }

        // Lukk leseren
        reader.close();
    }
}
```

Outputen for dette eksempelet vil være:

```
Bokhandelnavn: ABC Bokhandel
Adresse: Hovedgaten 1, Oslo
Tittel: Programming in Java
Forfatter: John Smith
Tittel: The Complete Guide to Web Development
Forfatter: Jane Doe
```

Du kan også bruke JSONObject- og JSONArray-klassene til å opprette og skrive ut JSON-data. For eksempel kan du legge til en ny bok i listen og skrive ut den oppdaterte JSON-filen ved å bruke følgende kode:

```Java
// Setter opp JSON-data for den nye boken
JSONObject newBook = new JSONObject();
newBook.put("title", "Data Structures in Java");
newBook.put("author", "Bob Johnson");

// Legger til den nye boken i listen over bøker
books.put(newBook);

// Skriver ut den oppdaterte JSON-filen
System.out.println(bookstore.toString());
```

Outputen vil være:

```JSON
{
    "name": "ABC Bokhandel",
    "address": "Hovedgaten 1, Oslo",
    "books": [
        {
            "title": "Programming in Java",
            "author": "John Smith"
        },
        {
            "title": "The Complete Guide to Web Development",
            "author": "Jane Doe"
        },
        {
            "title": "Data Structures in Java",
            "author": "Bob Johnson"
        }
    ]
}
```

## Dypdykk

JSON tillater også å bygge komplekse datastrukturer som kan være nyttige i mer avanserte programmer. Det er viktig å forstå forskjellen mellom JSON-objekter (representert med { }) og JSON-arrays (representert med [ ]). Mens JSON-objekter består av nøkkel-verdi-par, kan JSON-arrays inneholde flere verdier av samme type. Det er også viktig å være oppmerksom på hvordan man henter ut data fra disse forskjellige strukturene ved å bruke riktig syntaks.

En annen viktig egenskap ved JSON er dets evne til å representere nestede datastrukturer. Dette betyr at du kan ha JSON-objekter inni andre JSON-objekter eller JSON-arrays. Dette er spesi