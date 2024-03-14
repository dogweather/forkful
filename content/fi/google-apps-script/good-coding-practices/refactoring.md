---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:25.381901-07:00
description: "Ohjelmoinnin sanastossa refaktorointi viittaa olemassa olevan tietokonekoodin\
  \ uudelleenj\xE4rjest\xE4miseen\u2014muuttamalla sen toteutusta muuttamatta sen\
  \ ulkoista\u2026"
lastmod: '2024-03-13T22:44:56.107115-06:00'
model: gpt-4-0125-preview
summary: "Ohjelmoinnin sanastossa refaktorointi viittaa olemassa olevan tietokonekoodin\
  \ uudelleenj\xE4rjest\xE4miseen\u2014muuttamalla sen toteutusta muuttamatta sen\
  \ ulkoista\u2026"
title: Uudelleenkoodaus
---

{{< edit_this_page >}}

## Mikä ja miksi?

Ohjelmoinnin sanastossa refaktorointi viittaa olemassa olevan tietokonekoodin uudelleenjärjestämiseen—muuttamalla sen toteutusta muuttamatta sen ulkoista käytöstä—parantaakseen ei-toiminnallisia ominaisuuksia. Se on elintärkeä askel ohjelmoijille koodin luettavuuden parantamiseksi, monimutkaisuuden vähentämiseksi ja potentiaalisesti piilevien virheiden löytämiseksi, helpottaen ylläpitoa ja tulevan koodin skaalautuvuutta.

## Kuinka:

Google Apps Scriptissä, yleinen skenaario, joka hyötyy refaktoroinnista, on kömpelöiden skriptien yksinkertaistaminen, jotka vuorovaikuttavat Google Sheetsin tai Docsin kanssa. Alun perin skriptejä saatetaan kirjoittaa nopeasti ja likaisesti tulosten saamiseksi nopeasti. Ajan myötä, kun skripti kasvaa, se muuttuu hankalaksi. Käydään läpi esimerkki paremman luettavuuden ja tehokkuuden saavuttamiseksi refaktoroinnilla.

**Alkuperäinen skripti:**

```javascript
function logSheetNames() {
  var sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  for (var i = 0; i < sheets.length; i++) {
    Logger.log(sheets[i].getName());
  }
}
```

Tämä funktio kirjaa jokaisen Google Spreadsheetissä olevan arkkin nimen. Vaikka se toimii hyvin, se käyttää vanhentuneita JavaScript-käytäntöjä ja kaipaa selkeyttä.

**Refaktoroitu skripti:**

```javascript
function logSheetNames() {
  const sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  sheets.forEach(sheet => Logger.log(sheet.getName()));
}
```

Refaktoroidussa versiossa olemme vaihtaneet käyttämään `const` muuttujia muuttumattomille arvoille, tekevän tarkoituksemme selvemmäksi. Olemme myös käyttäneet `forEach`-metodia, modernimpaa ja tiiviimpää tapaa iteroivaan taulukoiden läpi, parantaen luettavuutta.

**Esimerkkitulostus (molemmille skripteille):**

Loggerissa näyttäisi jotakin tältä, olettaen että Google Sheets -dokumentissasi on kaksi arkkiä nimeltä "Expenses" ja "Revenue":

```
[20-04-2023 10:00:00: INFO] Expenses
[20-04-2023 10:00:01: INFO] Revenue
```

Refaktoroitu skripti saavuttaa saman tuloksen, mutta on siistimpi ja helpompi ymmärtää ensisilmäyksellä.

## Syväsukellus

Refaktorointi Google Apps Scriptissä perii osittain sen periaatteet laajemmasta ohjelmistotekniikan käytännöstä. Se tuli tunnetummaksi ja rakenteellisemmaksi käsitteeksi 1990-luvun lopulla, erityisesti Martin Fowlerin merkittävän kirjan "Refactoring: Improving the Design of Existing Code" (1999) ansiosta, joka tarjosi kattavan oppaan erilaisille refaktorointitekniikoille. Vaikka refaktoroinnin erityiskohdat voivat vaihdella ohjelmointikielten välillä niiden syntaktisten ja toiminnallisten erojen vuoksi, ydin tavoite pysyy samana: parantaa koodia muuttamatta sen ulkoista käytöstä.

Google Apps Scriptin kontekstissa refaktoroinnissa tärkeä näkökohta on Googlen asettamat palvelukiintiöt ja rajoitukset. Tehokkaasti refaktoroitu koodi ei vain lueta paremmin, vaan se myös toimii nopeammin ja luotettavammin näiden rajoitusten puitteissa. Esimerkiksi eräoperaatiot (`Range.setValues()` sijaan arvojen asettaminen yksi solu kerrallaan) voivat merkittävästi vähentää suoritusaikaa ja kiintiönkulutusta.

On tärkeää huomata, että tietyissä monimutkaisissa projekteissa Google Apps Script saattaa jäädä lyhyeksi juuri näiden rajoitusten vuoksi. Tällaisissa tapauksissa vaihtoehtojen, kuten Google Cloud Functions tai Apps Scriptin uudempi sisar, AppSheet, tutkiminen voisi tarjota parempaa skaalautuvuutta ja toiminnallisuutta.

Lopulta, vaikka refaktorointi on kriittinen taito Google Apps Script -projektien ylläpidossa ja parantamisessa, ympäristön rajoitusten ymmärtäminen ja vaihtoehtoisten ratkaisujen harkitseminen on yhtä tärkeää tehokkaan, luotettavan ja ylläpidettävän koodin toimittamisessa.
