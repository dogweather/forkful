---
title:                "Työskentely csv:n kanssa"
html_title:           "Javascript: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

CSV eli comma-separated values on yksi yleisesti käytetyistä tiedostomuodoista sisällön tallentamiseen ja jakamiseen. Se on erityisen hyödyllinen luku- ja kirjoitussovellusten välillä, kuten Microsoft Excel ja tietokantapalvelut. Työskentely CSV-tiedostojen kanssa on tärkeää monille eri aloille, kuten ohjelmistokehittäjille, liiketoiminta-analyytikoille ja tietokannan hallinnalle.

## Miten tehdä se

CSV-tiedostojen lukeminen ja kirjoittaminen Javascriptillä on yksinkertaista käyttämällä sisäänrakennettua "fs" -moduulia. Voit aloittaa lukemalla tiedoston "fs.readFileSync()" -funktiolla ja muuntaa sen taulukkoon käyttämällä "split()" -metodia. Tässä on yksinkertainen esimerkki:

```Javascript
// Luodaan muuttuja csv-tiedostolle
let csv = fs.readFileSync('tiedosto.csv', 'utf8');

// Muuta tiedoston sisältö taulukoksi rivien välillä
let taulukko = csv.split('\n');
```

Tämän jälkeen voit käsitellä tietoja taulukon avulla ja muuttaa ne halutulla tavalla. Esimerkiksi voit lajitella taulukon käyttämällä "sort()" -metodia tai suodattaa sitä käyttämällä "filter()" -metodia. Lopuksi voit tallentaa muokatun taulukon takaisin CSV-tiedostoon käyttämällä "join()" -metodia yhdessä "fs.writeFileSync()" -funktion kanssa.

```Javascript
// Esimerkki taulukon lajittelusta perustuen ensimmäiseen sarakkeeseen
taulukko.sort((a, b) => {
  // Asetetaan sarakkeiden välille pilkkuja
  let sarakkeetA = a.split(',');
  let sarakkeetB = b.split(',');
  
  // Vertaillaan ensimmäistä saraketta ja palautetaan järjestetty taulukko
  return sarakkeetA[0] - sarakkeetB[0];
});

// Esimerkki taulukon suodattamisesta vain tietyn ehdot täyttäville riveille
let suodatettuTaulukko = taulukko.filter(rivi => {
  let sarakkeet = rivi.split(',');
  return sarakkeet[3] === 'Kyllä'; // Suodatetaan kaikki kentät, joissa 4. sarake on "Kyllä"
});

// Tallennetaan muokattu taulukko takaisin CSV-tiedostoon
let muokattuCSV = suodatettuTaulukko.join('\n');
fs.writeFileSync('tiedosto-uusi.csv', muokattuCSV, 'utf8');
```

## Syvällinen sukellus

CSV-tiedostot voivat olla monimutkaisia ja niiden käsittelyssä voi esiintyä haasteita, kuten sarakkeiden tai rivien välillä olevat puuttuvat tiedot. On tärkeää olla varovainen tiedostojen käsittelyssä ja varmistaa, että tiedot tallennetaan oikein. Lisäksi voit joutua käsittelemään erilaisia merkistökoodauksia ja erikoismerkkejä kuten pilkkuja, lainausmerkkejä tai kaksoispisteitä.

Onnea matkaan kehittäessäsi Javascript-sovelluksia, jotka työskentelevät CSV-tiedostojen kanssa! Muista tarkistaa virheitä ja varmistaa, että tiedot tallentuvat oikein.

## Katso myös

- [Node.js fs -moduuli (englanniksi)](https://nodejs.org/api/fs.html)
- [Javascriptin tietotyypit: Taulukko (engl