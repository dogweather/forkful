---
title:                "Testien kirjoittaminen"
date:                  2024-02-01T22:09:05.042173-07:00
model:                 gpt-4-0125-preview
simple_title:         "Testien kirjoittaminen"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/google-apps-script/writing-tests.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Testien kirjoittaminen Google Apps Scriptillä (GAS) tarkoittaa automatisoitujen skriptien luomista koodisi käyttäytymisen varmistamiseksi, jotta ne toimivat odotetulla tavalla eri olosuhteissa. Ohjelmoijat tekevät näin varhaisvaiheessa esiintyvien virheiden havaitsemiseksi, koodin laadun parantamiseksi ja päivitysten sekä ylläpidon helpottamiseksi.

## Miten:

Vaikka Google Apps Scriptillä ei ole sisäänrakennettua testausympäristöä kuten joissakin muissa ohjelmointiympäristöissä, voit silti kirjoittaa ja suorittaa testejä hyödyntämällä yksinkertaisia GAS-funktioita tai integroimalla ulkoisia testikirjastoja, kuten `QUnit`. Tässä on perusesimerkki, joka käyttää yksinkertaista GAS-funktiota toisen skriptisi funktion testaamiseen:

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("Testi epäonnistui: add(2, 3) pitäisi olla 5, mutta oli " + result);
  } else {
    Logger.log("Testi onnistui!");
  }
}
```

`testAdd()` suorittaminen kirjaa "Testi onnistui!", jos `add`-funktio toimii oikein, tai heittää virheen, jos se ei toimi. Monimutkaisempaa lähestymistapaa varten QUnitin integroiminen Google Apps Scriptiin vaatii hieman enemmän vaiheita, mutta tarjoaa tehokkaan testausympäristön. Esimerkki QUnit-testiasetuksesta näyttää tältä:

1. Sisällytä QUnit-kirjasto projektiisi.
2. Luo testi HTML-tiedosto QUnit-testien suorittamista varten.
3. Kirjoita testitapauksia käyttäen QUnitin syntaksia.

Tässä on esimerkki QUnitin käytöstä:

```javascript
// Sisällytä QUnit linkittämällä se HTML-tiedostoon, jota käytät testiesi suorittamiseen

QUnit.test("Testataan add-funktiota", function (assert) {
  var result = add(2, 3);
  assert.equal(result, 5, "add(2, 3) pitäisi palauttaa 5");
});
```

Nähdäksesi tulokset, avaa HTML-tiedosto GAS-komentosarjaeditorissa tai julkaise se web-sovelluksena.

## Syväsukellus

Historiallisesti Google Apps Scriptin testaaminen on jäänyt jokseenkin sivuun, luultavasti alustan alkuperän ja pääkäyttötapauksien keskittymisen nopeisiin, pienimuotoisiin automaatiotehtäviin eikä suuriin sovelluksiin vuoksi. GAS ei siis tarjoa samoja vankkoja testauskehyksiä ja -työkaluja kuin perinteisemmissä ohjelmointiympäristöissä. Kuitenkin yhteisö on sopeutunut sisällyttämällä avoimen lähdekoodin kirjastoja ja hyödyntämällä luovasti Googlen olemassa olevia työkaluja.

Kirjastojen, kuten QUnitin, käyttö edustaa merkittävää askelta eteenpäin, mutta tuo mukanaan omat haasteensa, kuten sopivan testausympäristön pystyttämisen ja lisäsyntaksin opettelun. Kuitenkin niille, jotka ovat sitoutuneita kehittämään monimutkaisempia ja luotettavampia sovelluksia GAS:lla, ponnistelut ovat sen arvoisia.

Vaihtoehdot, kuten yksinkertaisten GAS-funktioiden käyttäminen testaamiseen, tarjoavat helppokäyttöisyyttä ja integroitumista GAS-ympäristöön ilman lisäriippuvuuksia, mutta puuttuvat kattavia testausominaisuuksia ja kykyä helposti skaalautua projektisi kasvaessa. Työkalut, kuten clasp (Google Apps Script Command Line Interface), voivat helpottaa edistyneempiä työnkulkuja, mukaan lukien testaaminen, antamalla kehittäjille mahdollisuuden koodata suosimassaan IDE:ssä, tuoden mukanaan mahdollisuuksia integroitua saumattomammin ulkoisiin testikehyksiin.

Yhteenvetona, vaikka GAS ei ehkä tarjoa natiivia tukea monimutkaisille testeille suoraan paketista, sen joustavuus ja yhteisön innovatiiviset lähestymistavat tarjoavat toteuttamiskelpoisia polkuja varmistamaan, että skriptisi ovat vankkoja, luotettavia ja valmiita mihin tahansa tehtävään.
