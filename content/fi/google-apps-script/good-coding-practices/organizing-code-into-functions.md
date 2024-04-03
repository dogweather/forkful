---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:38.053732-07:00
description: "Koodin j\xE4rjest\xE4minen funktioihin tarkoittaa Google Apps Script\
  \ -koodisi rakenteen j\xE4rjest\xE4mist\xE4 erottamalla loogiset osat toisistaan\
  \ selv\xE4sti erottuviksi\u2026"
lastmod: '2024-03-13T22:44:56.103526-06:00'
model: gpt-4-0125-preview
summary: "Koodin j\xE4rjest\xE4minen funktioihin tarkoittaa Google Apps Script -koodisi\
  \ rakenteen j\xE4rjest\xE4mist\xE4 erottamalla loogiset osat toisistaan selv\xE4\
  sti erottuviksi lohkoiksi, joista jokainen suorittaa tietyn teht\xE4v\xE4n."
title: "Koodin j\xE4rjest\xE4minen funktioihin"
weight: 18
---

## Kuinka:
Google Apps Scriptissä, joka perustuu JavaScriptiin, funktiot määritellään käyttämällä `function`-avainsanaa, jonka jälkeen tulee uniikki funktion nimi, sulkumerkit `()` jotka voivat sisältää parametreja, ja aaltosulkeet `{}` jotka kapseloivat funktion koodilohkon. Tässä on perusesimerkki:

```javascript
function greetUser() {
  var user = Session.getActiveUser().getEmail();
  Logger.log('Terve, ' + user + '!');
}

greetUser();
```

Esimerkkitulostus:

```
Terve, joku@example.com!
```

Nyt, tarkastellaan käytännöllisempää esimerkkiä liittyen Google Sheetsiin, jossa erotamme toiminnallisuuden kahteen funktioon: yksi lomakkeen alustamiseen ja toinen sen täyttämiseen tiedoilla.

```javascript
function setupSheet() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet = ss.getSheets()[0];
  sheet.setName('Myyntidata');
  sheet.appendRow(['Tuote', 'Määrä', 'Hinta']);
}

function populateSheet(data) {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName('Myyntidata');
  data.forEach(function(row) {
    sheet.appendRow(row);
  });
}

// Alusta tietojen taulukko
var salesData = [
  ['Vempaimet', 15, 2.5],
  ['Hilavitkuttimet', 8, 3.75]
];

// Suorita funktiot
setupSheet();
populateSheet(salesData);
```

Tässä esimerkissä `setupSheet` valmistelee lomakkeen, ja `populateSheet` ottaa vastaan myyntidatan taulukon ja täyttää lomakkeen sillä. Huolet erillisinä pitäminen tekee koodista siistimpää ja mukautuvampaa muutoksille.

## Syvä sukellus
Koodin jakaminen funktioihin ei ole uusi tai ainutlaatuinen konsepti Google Apps Scriptille; se on perusoletus ohjelmoinnissa lähes kaikissa ohjelmointikielissä. Historiallisesti funktiot kehittyivät matemaattisesta konseptista, joka kartoitti syötteet tuotoksiin, ja tästä tuli perustavanlaatuinen osa rakenteellista ohjelmointia. Tämä lähestymistapa edistää modulaarisuutta ja koodin uudelleenkäytettävyyttä, tarjoten selkeät polut yksittäisten skriptiosien testaamiseen.

Google Apps Script, ollessaan JavaScript-pohjainen, hyötyy merkittävästi JavaScriptin ensiluokkaisista funktioista, jotka mahdollistavat funktioiden välittämisen argumentteina, palauttamisen muista funktioista ja sijoittamisen muuttujiin. Tämä ominaisuus avaa kehittyneitä malleja kuten takaisinkutsut ja funktionaalinen ohjelmointi, vaikkakin nämä mallit voivat tuoda mukanaan monimutkaisuutta, joka voi olla tarpeetonta yksinkertaisissa automaatiotehtävissä Google Apps Scriptissä.

Suuremmissa projekteissa tai monimutkaisemmissa sovelluksissa kehittäjät saattavat tutkia JavaScriptin uudempia ominaisuuksia kuten nuolifunktioita, async/await asynkronisiin operaatioihin ja jopa TypeScriptiä staattiseen tyypitykseen. Erityisesti TypeScript, joka voidaan kääntää suoritettavaksi Google Apps Scriptinä, tarjoaa kehittäjille mahdollisuuden hakea tiukempaa tyypin tarkastusta ja edistyneitä oliosuuntautuneita ominaisuuksia.

Kuitenkin, suurimmaksi osaksi skriptaus tarpeita varten Google Apps -suite:ssa, pysytteleminen yksinkertaisissa, hyvin järjestetyissä funktioissa kuten esitetty tarjoaa vankan perustan. Kyse on aina tasapainottelusta kehittyneiden ominaisuuksien tehokkaaseen hyödyntämiseen ja yksinkertaisuuden säilyttämiseen helposti ylläpidettävyyden ja luettavuuden kannalta.
