---
title:                "Java: Tekstitiedoston kirjoittaminen"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Text-tiedostojen kirjoittaminen on olennainen osa ohjelmointia monissa eri tilanteissa. Se mahdollistaa tietojen tallentamisen ja käsittelemisen pitkäaikaisesti ja helposti vaihdettavan formaatin avulla. Ohjelmoijan on tärkeää osata kirjoittaa ja muokata text-tiedostoja, jotta työnkulku sujuisi mahdollisimman tehokkaasti ja laadukkaasti.

## Miten

Text-tiedoston kirjoittaminen Javalla on helppoa ja suoraviivaista. Se vaatii muutaman yksinkertaisen vaiheen:

1. Alusta FileOutputStream muuttuja tiedoston nimen ja polun kanssa
2. Alusta OutputStreamWriter muuttuja käyttäen FileOutputStreamia parametrina
3. Käytä OutputStreamWriterin "write" metodia kirjoittamaan haluamasi teksti tiedostoon
4. Käytä OutputStreamWriterin "close" metodia lopettaaksesi kirjoittamisen ja vapauttamalla resurssit

Alla olevassa esimerkissä luodaan "testitiedosto.txt" ja kirjoitetaan siihen "Tervetuloa Java-ohjelmointiblogiin!". Koodin tulosteena luodaan uusi tiedosto ja kirjoitettu teksti tallennetaan siihen:

```Java
public class TextFileWriter {
    public static void main(String[] args) {
        try {
            FileOutputStream file = new FileOutputStream("testitiedosto.txt");
            OutputStreamWriter writer = new OutputStreamWriter(file);
            writer.write("Tervetuloa Java-ohjelmointiblogiin!");
            writer.close();
            System.out.println("Tiedosto kirjoitettu onnistuneesti!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Tulosteena saat "Tiedosto kirjoitettu onnistuneesti!" konsoliin. Jos tarkastelet projektisi tiedostoja, löydät sieltä luomasi "testitiedosto.txt".

## Syväsukellus

Text-tiedostojen kirjoittaminen perustuu Output Stream -luokkaan, jota voidaan käyttää kirjoittamaan raaka dataa tai merkkejä. Output Stream -luokka on abstrakti luokka, ja sen erityiset aliluokat, kuten FileOutPutStream, ovat vastuussa kirjoittamisesta tiettyyn lähdeobjektiin, kuten tiedostoon.

Output Streamin käyttöön liittyy myös "bufferointi", joka helpottaa tiedon virtausta ja parantaa suorituskykyä. Tässä blogikirjoituksessa käytimme OutputStreamWriteria, joka on suunniteltu merkkien kirjoittamiseen bittivirtaan. Tämä mahdollistaa tekstin kirjoittamisen tiedostoon tavujen sijaan, mikä tekee prosessista paljon helpomman ja luettavamman ohjelmoijille.

## Katso myös

- [Input/Output Stream Javassa](https://www.tutorialspoint.com/java/java_files_io.htm)
- [Java Text File Reader](https://www.javatpoint.com/java-read-file)
- [Java FileWriter luokka](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)