---
title:                "Java: Työskentely csv-tiedostojen kanssa"
simple_title:         "Työskentely csv-tiedostojen kanssa"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi käyttää CSV-tiedostoja Java-ohjelmoinnissa?

CSV-tiedostot eli comma-separated values -tiedostot ovat yleinen ja helppokäyttöinen tapa tallentaa tietoa taulukkomuodossa. Ne ovat erityisen käteviä silloin, kun haluamme tallentaa ja käsitellä suuria määriä tietoa, esimerkiksi liiketoimintatarkoituksiin. Java-ohjelmoijana CSV-tiedostot voivat olla hyvin hyödyllisiä, sillä Java tarjoaa valmiita kirjastoja ja toimintoja CSV-tiedostojen käsittelyyn.

## Näin käytät CSV-tiedostoja Javalla

Jotta voimme käsitellä CSV-tiedostoja Javalla, tarvitsemme ensin jonkin kirjaston, joka lukee ja kirjoittaa CSV-tiedostoja. Yksi suosittu vaihtoehto on Apache Commons CSV -kirjasto. Otetaan esimerkiksi seuraava CSV-tiedosto:

```Java
Nimi, Ikä, Kaupunki
Matti, 25, Helsinki
Anna, 30, Tampere
Timo, 35, Turku
```
Voimme lukea tämän tiedoston seuraavalla koodilla:

```Java
try {
    // Luodaan lukija ja avataan tiedosto
    Reader reader = Files.newBufferedReader(Paths.get("tiedosto.csv"));

    // Luodaan CSVParser, joka käyttää ensimmäistä riviä taulukon otsikkorivinä
    CSVParser parser = new CSVParser(reader, CSVFormat.DEFAULT.withHeader());

    // Käydään läpi rivejä ja tulostetaan tiedot
    for (CSVRecord record : parser) {
        String nimi = record.get("Nimi");
        int ika = Integer.parseInt(record.get("Ikä"));
        String kaupunki = record.get("Kaupunki");
        System.out.println(nimi + " on " + ika + " vuotta vanha ja asuu kaupungissa " + kaupunki);
    }
} catch (IOException e) {
    e.printStackTrace();
}
```
Koodin tuloste on seuraava:

```
Matti on 25 vuotta vanha ja asuu kaupungissa Helsinki
Anna on 30 vuotta vanha ja asuu kaupungissa Tampere
Timo on 35 vuotta vanha ja asuu kaupungissa Turku
```
Voimme myös kirjoittaa uuden CSV-tiedoston käyttämällä samoja kirjastoja. Tässä esimerkissä luomme uuden listan henkilöistä ja kirjoitamme sen CSV-tiedostoksi:

```Java
try {
    // Alustetaan lista henkilöistä
    List<Person> persons = new ArrayList<>();
    persons.add(new Person("Lea", 45, "Oulu"));
    persons.add(new Person("Jussi", 40, "Jyväskylä"));
    persons.add(new Person("Katri", 35, "Kuopio"));

    // Luodaan kirjoittaja ja avataan tiedosto
    Writer writer = Files.newBufferedWriter(Paths.get("uusi_tiedosto.csv"));

    // Kirjoitetaan otsikkorivi
    String[] header = {"Nimi", "Ikä", "Kaupunki"};
    CSVPrinter printer = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(header));

    // Käydään läpi lista ja kirjoitetaan tiedot jokaiselle riville
    for (Person person : persons) {
        printer.printRecord(person.getName(), person.getAge(), person.getCity());
    }

    // Suljetaan kirjoittaja
    writer.close();
} catch (IOException e) {
    e.printStackTrace();
}
```

## CSV-tiedostojen syvällisempi tarkastelu

CSV-tiedostojen käsittely Javalla voi olla hieman monimutkaista, sillä tiedostoja ei aina pysty lukemaan tai kirjoittamaan suoraan käyttäen Javan valmiita toimintoja. Tiedostojen muoto