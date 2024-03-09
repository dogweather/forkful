---
title:                "Työskentely CSV:n kanssa"
date:                  2024-03-08T21:57:24.534428-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

CSV-tiedostojen (pilkuilla erotetut arvot) käsittely käsittää teksti-tiedostojen jäsennyksen ja luonnin, missä jokainen rivi sisältää pilkuilla erotettuja arvoja. Ohjelmoijat tekevät tämän mahdollistaakseen tietojen vaihdon eri sovellusten välillä tai helpottaakseen tietojen tallennusta kevyessä, ihmissilmällä luettavassa muodossa.

## Kuinka:

CSV-tiedostojen käsittelyyn Dartissa voit joko käsitellä tekstin manuaalisesti tai käyttää kolmannen osapuolen kirjastoja tehtävän yksinkertaistamiseksi. Tässä katsomme molempia lähestymistapoja.

### CSV:n manuaalinen jäsennys

Jos tarpeesi ovat yksinkertaisia, saatat päättää jäsennellä CSV-merkkijonon manuaalisesti. Tämä voidaan saavuttaa käyttämällä Dart:n perus merkkijonojen käsittelyn toimintoja:

```dart
void main() {
  // Esimerkki CSV-data
  String csvData = "Nimi,Ikä,Sähköposti\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Jaetaan CSV-data riveihin
  List<String> rivit = csvData.split('\n');
  
  // Jokaisen rivin jäsennys
  List<Map<String, String>> data = [];
  List<String> otsikot = rivit.first.split(',');
  
  for (var i = 1; i < rivit.length; i++) {
    List<String> rivi = rivit[i].split(',');
    Map<String, String> tietue = {};
    for (var j = 0; j < otsikot.length; j++) {
      tietue[otsikot[j]] = rivi[j];
    }
    data.add(tietue);
  }
  
  // Tulostetaan jäsennetty data
  print(data);
}

// Esimerkki tuloste:
// [{Nimi: John Doe, Ikä: 30, Sähköposti: john@example.com}, {Nimi: Jane Smith, Ikä: 25, Sähköposti: jane@example.com}]
```

### Kolmannen osapuolen kirjaston käyttö: `csv`

Monimutkaisempiin tilanteisiin tai koodisi yksinkertaistamiseksi voit käyttää suosittua kolmannen osapuolen kirjastoa kuten `csv`. Lisää se projektiisi lisäämällä `csv: ^5.0.0` (tai viimeisin versio) `pubspec.yaml` tiedostosi `dependencies`-kohtaan. Käytä sitä seuraavasti:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Nimi,Ikä,Sähköposti\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Käytetään CsvToListConverteria CSV-datan jäsennykseen
  List<List<dynamic>> listaData = const CsvToListConverter().convert(csvData);
  
  // Ensimmäinen listan alkio sisältää otsikot
  List<String> otsikot = listaData.first.map((item) => item.toString()).toList();
  
  // Poistetaan otsikkorivi ennen edelleen käsittelyä
  listaData.removeAt(0);
  
  // Muunnetaan List<Map<String, dynamic>>:ksi rakenteellisempaan muotoon
  List<Map<String, dynamic>> karttatutData = listaData.map((lista) {
    Map<String, dynamic> kartta = {};
    for (int i = 0; i < otsikot.length; i++) {
      kartta[otsikot[i]] = lista[i];
    }
    return kartta;
  }).toList();
  
  // Tulostetaan mapattu data
  print(karttatutData);
}

// Esimerkki tuloste:
// [{Nimi: John Doe, Ikä: 30, Sähköposti: john@example.com}, {Nimi: Jane Smith, Ikä: 25, Sähköposti: jane@example.com}]
```

Molemmat menetelmät havainnollistavat, miten työskennellä CSV-datan kanssa: ensimmäinen manuaalisesti, oppimistarkoituksessa tai kun käsitellään erittäin yksinkertaisia CSV-rakenteita; toinen, käyttämällä tehokasta kirjastoa, joka yksinkertaistaa jäsennystä ja pystyy käsittelemään CSV-muotoilun eri monimutkaisuuksia.
