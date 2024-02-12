---
title:                "Tekstin etsiminen ja korvaaminen"
aliases:
- /fi/google-apps-script/searching-and-replacing-text.md
date:                  2024-02-01T22:01:18.430569-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekstin etsiminen ja korvaaminen"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/google-apps-script/searching-and-replacing-text.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstin etsiminen ja korvaaminen Google Apps Scriptilla käsittää tiettyjen merkkijonojen ohjelmallisen tunnistamisen dokumentissa, laskentataulukossa tai missä tahansa muussa Google Apps -sisältötyypissä ja niiden korvaamisen toisilla tekstiarvoilla. Ohjelmoijat hyödyntävät tätä toiminnallisuutta suurten sisältömäärien automaattisessa muokkauksessa, yleisten virheiden korjaamisessa, terminologian standardoinnissa eri dokumenteissa tai dynaamisten tietojen lisäämisessä malleihin.

## Kuinka:

Google Apps Script tarjoaa suoraviivaisen tavan etsiä ja korvata tekstiä, erityisesti Google Docs ja Sheets -sovelluksissa. Alla on esimerkkejä molemmista.

### Google Docs:

Tekstin etsimisessä ja korvaamisessa Google-dokumentissa toimit ensisijaisesti `DocumentApp`-luokan kanssa.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // Tietyn lauseen etsimiseen ja korvaamiseen
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// Käyttö
searchReplaceInDoc();
```

Tämä koodinpätkä etsii kaikki aktiivisessa Google-dokumentissa olevat `'searchText'`-esiintymät ja korvaa ne `'replacementText'`-tekstillä.

### Google Sheets:

Samoin Google Sheets -sovelluksessa voit käyttää `SpreadsheetApp`ia suorittamaan etsintä- ja korvaustoimintoja:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // Etsintä ja korvaus aktiivisessa taulukossa
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// Käyttö
searchReplaceInSheet();
```

Tässä esimerkissä `createTextFinder('searchText')` etsii aktiivisesta taulukosta 'searchText', ja `replaceAllWith('replacementText')` korvaa kaikki esiintymät 'replacementText'-tekstillä.

## Syväsukellus

Etsi- ja korvaustoiminnallisuus Google Apps Scriptissa on voimakkaasti sidoksissa sen web-pohjaiseen luonteeseen, mikä mahdollistaa skriptien saumattoman tekstin manipuloinnin eri Google Apps -sovelluksissa. Historiallisesti tämä kyky juontaa juurensa laajemmasta tekstinkäsittelyn ja -manipuloinnin kontekstista ohjelmoinnissa, missä säännölliset lausekkeet ja merkkijonofunktiot kielissä, kuten Perl ja Python, ovat asettaneet korkean standardin joustavuudelle ja tehokkuudelle.

Vaikka Google Apps Scriptin etsi- ja korvaustoiminnallisuus on tehokas yksinkertaisiin korvauksiin, se ei tarjoa yhtä kattavia säännöllisten lausekkeiden toimintoja kuin jotkin muut kielet. Esimerkiksi vaikka `createTextFinder` Google Sheets -sovelluksessa mahdollistaa perussäännöllisten lausekkeiden käytön, monimutkaisten kuvioiden vastaavuuden ja käsittelyn vaihtoehdot ovat rajoitetut verrattuna Perliin tai Pythoniin.

Monimutkaisempien tekstinkäsittelytarpeiden osalta ohjelmoijat saattavat turvautua viemään Google Docs- tai Sheets-sisällön formaattiin, jota voidaan käsitellä ulkoisesti tehokkaammilla kielillä, tai käyttämään Google Apps Scriptia kutsumaan ulkoisia API:ita tai palveluita, jotka tarjoavat monimutkaisempia tekstinkäsittelyominaisuuksia.

Huolimatta näistä rajoituksista, tyypillisissä etsi- ja korvaustehtävissä Google Apps -ekosysteemissä Google Apps Script tarjoaa yksinkertaisen, tehokkaan ja helposti integroitavan ratkaisun, joka on räätälöity Google tuottavuustyökalusarjan automatisointi- ja skriptausvaatimuksiin.
