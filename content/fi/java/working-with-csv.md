---
title:                "Työskentely csv:n kanssa"
html_title:           "Java: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-csv.md"
---

{{< edit_this_page >}}

Miksi: Miksi työskennellä CSV: n kanssa?

CSV (Comma-Separated Values) on yleisesti käytetty tiedostomuoto tietojen tallentamiseen ja jakamiseen. Se on helppo lukea ja kirjoittaa, mikä tekee siitä houkuttelevan vaihtoehdon tietojen hallintaan. Joten, jos olet kehittäjä, joka käsittelee paljon tietoa, CSV voi olla erinomainen työkalu työssäsi.

Miten: Esimerkkejä koodista ja tulosteista

CSV-tiedostojen käsittely Java-ohjelmoinnissa on yksinkertaista ja suoraviivaista. Käytämme esimerkkinä yksinkertaista CSV-tiedostoa, joka sisältää käyttäjien tietoja, kuten nimi, ikä ja sähköposti.

Ensinnäkin, tuodaan tarvittavat luokat "csv- ja io" Java-paketista:

```Java
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
```

Seuraavaksi luodaan "writeCSV" -metodi, joka luo uuden CSV-tiedoston ja lisää siihen käyttäjien tiedot:

```Java
public static void writeCSV() {
   try {
       FileWriter fw = new FileWriter("users.csv");
       CSVPrinter csvPrinter = new CSVPrinter(fw, CSVFormat.DEFAULT);
       
       //Luodaan lista käyttäjien tiedoista
       List<String> usersData = new ArrayList<>();
       usersData.add("Matti, 30, matti@matti.com");
       usersData.add("Katri, 25, katri@katri.com");
       usersData.add("Juha, 35, juha@juha.com");
       
       //Lisätään lista CSV-tiedostoon
       for(String userData : usersData) {
           csvPrinter.printRecord(userData);
       }
       
       //Suljetaan CSV-tiedosto
       csvPrinter.close();
       
       //Tulostetaan onnistunut viesti
       System.out.println("CSV-tiedosto luotu ja käyttäjien tiedot lisätty.");
   } catch (IOException e) {
       e.printStackTrace();
   }
}
```

Tämän jälkeen voimme kutsua "writeCSV" -metodia ja nähdä tulosteen, että CSV-tiedosto on luotu ja käyttäjien tiedot on lisätty siihen:

```Java
public static void main(String[] args) {
   writeCSV();
}
```

Tuloste:

```Java
CSV-tiedosto luotu ja käyttäjien tiedot lisätty.
```

Voit myös lukea tiedot CSV-tiedostosta käyttämällä "readAll" -metodia ja tulostaa ne konsoliin:

```Java
public static void readCSV() {
   try {
       File csvFile = new File("users.csv");
       CSVParser csvParser = CSVParser.parse(csvFile, Charset.defaultCharset(), CSVFormat.DEFAULT);
       
       //Käydään läpi kaikki CSV-tiedoston rivit ja tulostetaan ne konsoliin
       for (CSVRecord csvRecord : csvParser) {
           System.out.println("Nimi: " + csvRecord.get(0));
           System.out.println("Ikä: " + csvRecord.get(1));
           System.out.println("Sähköposti: " + csvRecord.get(2));
       }
   } catch (IOException e) {
       e.printStackTrace();
   }
}
```

Deep Dive: CSV-tiedostojen käsittelyssä on tärkeää huolehtia tiedostojen muodostuksesta ja tietojen sijoittamisesta oikeisiin kenttiin. Voit myös käyttää erilaisia ​​CSVFormaatteja tarpeidesi mukaan, kuten CSVFormat.TDF (Tab-delimited format) tai CSVFormat.RFC4180 (RFC 4180 standard format). Huomaa myös, että CSV-tiedostojen lukemisessa tulokset palautetaan CSVRecord-olioina, joid