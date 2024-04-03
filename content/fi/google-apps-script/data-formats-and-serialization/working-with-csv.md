---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:45.467398-07:00
description: "CSV-tiedostojen (pilkuilla erotetut arvot) k\xE4sittely Google Apps\
  \ Scriptill\xE4 sis\xE4lt\xE4\xE4 tekstimuotoisten tiedostojen lukemisen, muokkaamisen\
  \ ja\u2026"
lastmod: '2024-03-13T22:44:56.122716-06:00'
model: gpt-4-0125-preview
summary: "CSV-tiedostojen (pilkuilla erotetut arvot) k\xE4sittely Google Apps Scriptill\xE4\
  \ sis\xE4lt\xE4\xE4 tekstimuotoisten tiedostojen lukemisen, muokkaamisen ja kirjoittamisen,\
  \ joissa jokainen rivi esitt\xE4\xE4 datatietuetta, jonka arvot on erotettu pilkuilla."
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

## Kuinka:


### CSV-datan lukeminen
CSV-datatiedoston lukemiseksi Google Drivesta, sinun tulee ensin hankkia tiedoston sisältö merkkijonona ja sitten jäsentää se. Google Apps Script tekee tiedoston sisällön noutamisen suoraviivaiseksi DriveApp-palvelun avulla.

```javascript
function readCSV() {
  var fileId = 'YOUR_FILE_ID_HERE'; // Korvaa todellisella tiedostotunnisteella
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // Kirjaa jokaisen rivin solut
  }
}
```

### CSV-datan kirjoittaminen
CSV:n luominen ja siihen kirjoittaminen merkitsee merkkijonon rakentamista, jossa on pilkulla erotetut arvot ja rivinvaihdot, jonka jälkeen tallennetaan tai viedään se. Tämä esimerkki osoittaa uuden CSV-tiedoston luomisen Google Driveen.

```javascript
function writeCSV() {
  var folderId = 'YOUR_FOLDER_ID_HERE'; // Korvaa kansion tunniste uudessa tiedostossa, johon tiedosto luodaan
  var csvContent = "Nimi,Ikä,Ammatti\nJohn Doe,29,Insinööri\nJane Smith,34,Designer";
  var fileName = "example.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### Esimerkki tuloste
Kun kirjataan rivi soluja CSV:stä lukiessa:

```plaintext
[John, 29, Insinööri]
[Jane, 34, Designer]
```

Kirjoittaessa luodaan tiedosto nimeltä "example.csv", jonka sisältö on:

```plaintext
Nimi,Ikä,Ammatti
John Doe,29,Insinööri
Jane Smith,34,Designer
```

## Syväsukellus
Historiallisesti CSV-tiedostoja on suosittu niiden yksinkertaisuuden ja ihmisen luettavuuden vuoksi, mikä tekee niistä saavutettavia myös ei-ohjelmoijille ja hyödyllisiä nopeisiin datan tarkastustehtäviin. Kuitenkin Google Apps Script toimii Googlen ekosysteemin piirissä, jossa Google Sheets toimii tehokkaana, käyttäjäystävällisenä vaihtoehtona CSV-käsittelylle. Sheets ei tarjoa ainoastaan GUI:ta datan muokkamiseen vaan myös tukee kompleksisia kaavoja, tyylittelyä ja monia muita ominaisuuksia, joita raaka CSV ei tarjoa.

Siitä huolimatta Google Sheetsin tarjoamista eduista huolimatta suora CSV-käsittely Google Apps Scriptillä pysyy tärkeänä automatisoiduissa tehtävissä, erityisesti kun käsitellään ulkoisia järjestelmiä, jotka tuottavat tai vaativat dataa CSV-muodossa. Esimerkiksi integrointi vanhoihin järjestelmiin, datan vieminen käyttöön muissa sovelluksissa tai esikäsittely ennen datan syöttämistä Google Sheetsiin.

Lisäksi, Google Apps Scriptin kyky käsitellä CSV-tiedostoja voidaan laajentaa Utilities-palvelulla edistyneisiin koodaustarpeisiin tai liittää ulkoisiin API:eihin muunnos-, jäsentämis- tai validointitehtäviin. Kuitenkin suurten datamäärien käsittelyssä tai monimutkaisten manipulaatioiden tarpeessa kannattaa harkita Google Sheets API:en käyttöä tai tutkia BigQuerya vankempiin dataprosessointikyvykkyyksiin.

Vaikka yksinkertaisuus pysyy keskeisenä syynä CSV:n suosiolle, nämä vaihtoehdot tarjoavat rikkaamman joukon ominaisuuksia datan käsittelyyn laajassa Google Cloud -ekosysteemissä.
