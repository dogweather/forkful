---
title:                "Uuden projektin aloittaminen"
aliases: - /fi/google-apps-script/starting-a-new-project.md
date:                  2024-02-01T22:02:55.945859-07:00
model:                 gpt-4-0125-preview
simple_title:         "Uuden projektin aloittaminen"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/google-apps-script/starting-a-new-project.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Uuden projektin aloittaminen Google Apps Scriptissä (GAS) tarkoittaa komentosarjatiedoston alustamista Google-ekosysteemissä (Google Drive, Docs, Sheets jne.) tehtävien automatisoimiseksi tai Google Apps -toimintojen laajentamiseksi. Ohjelmoijat usein lähtevät tälle matkalle työnkulun tehostamiseksi, Google-palveluiden ohjelmalliseksi manipuloinniksi tai mukautettujen lisäosien luomiseksi, mikä säästää aikaa ja hyödyntää Googlen infrastruktuurin voimaa.

## Kuinka:

Aloittaaksesi uuden projektin Google Apps Scriptissä, sinulla on muutama aloituskohta, mutta keskitytään suorimpaan menetelmään: skriptin luominen Google Drivessä.

1. **Projektin luominen Google Drivessä**
   - Siirry Google Driveen (drive.google.com).
   - Napsauta "+ Uusi" > "Lisää" > "Google Apps Script".
   - Uusi skriptiprojekti avautuu editorissa. Oletuksena se sisältää `Code.gs`-tiedoston, jossa on näytefunktio `myFunction`.

2. **Projektisi asettaminen**
   - Anna projektillesi selkeä nimi. Napsauta vasemmalla yläkulmassa "Nimetön projekti" ja anna sille merkityksellinen nimi.
   - Kirjoita yksinkertainen funktio `Code.gs`-tiedostoon, jotta saat tuntumaa siitä:

```javascript
function helloWorld() {
  Logger.log('Hei, maailma!');
}
```

   - Suorita `helloWorld` valitsemalla funktio pudotusvalikosta toistonapin (▶) vieressä ja napsauttamalla sitä. Tämä suorittaa funktion.

3. **Lokien tarkastelu**
   - Nähdäksesi `Logger.log`-tulosteen, mene "Näkymä" > "Lokit", tai paina `Ctrl + Enter`. Sinun pitäisi nähdä "Hei, maailma!" lokeissa.

Onnittelut, olet juuri onnistuneesti aloittanut uuden projektin Google Apps Scriptissä ja suorittanut yksinkertaisen funktion!

## Syväsukellus

Google Apps Scriptin perustaminen vuonna 2009 tarjosi tehokkaan, mutta lähestyttävän alustan sekä kehittäjille että ei-kehittäjille Google-palveluiden automatisointiin, laajentamiseen ja rakentamiseen. Toisin kuin perinteiset ohjelmointiympäristöt, GAS tarjoaa yksinkertaisuuden ja integraation ainutlaatuisen sekoituksen suoraan Google-ekosysteemissä, ilman tarvetta ulkoisille palvelimille tai asetuksille. Tämä palvelinton suoritusmalli yksinkertaistaa suuresti projektien käyttöönottoa ja hallintaa.

Historiallisesti GAS oli jossain määrin rajoittunut suoritusympäristönsä ja kielen version suhteen, usein jäljessä nykyisistä JavaScript-standardeista. Kuitenkin, viimeaikaiset päivitykset ovat tuoneet modernin JavaScript-syntaksin (ECMAScript 2015+) GAS:iin, tehden siitä mieluisamman kehittäjille, jotka ovat tottuneet nykyaikaisiin kehityskäytäntöihin.

Vaikka GAS sijaitsee ainutlaatuisessa asemassa vuorovaikutuksessa Google-palveluiden kanssa, on olemassa vaihtoehtoisia lähestymistapoja intensiivisempiin tai tiettyihin tarpeisiin. Esimerkiksi Google Cloud Functions ja Google Cloud Platform (GCP) tarjoavat robustimman ja skaalautuvamman ratkaisun monimutkaisten työnkulkujen käsittelyyn, suurten tietoaineistojen prosessointiin ja ulkoisten API:en integrointiin. Nämä alustat sallivat ohjelmoinnin eri kielillä (esim. Python, Go, Node.js) ja tarjoavat suurempia laskennallisia resursseja.

Siitä huolimatta, Google Apps -tehtäviin, automaatioon ja nopeaan kehitykseen tässä ekosysteemissä sidoksissa oleville tehtäville, Google Apps Script pysyy vertaansa vailla olevana työkaluna käytön helppouden ja integraation syvyyden suhteen. Sen saatavuus suoraan Google Drivesta ja saumaton yhteys Google-palveluihin tekevät siitä käytännöllisen valinnan laajalle valikoimalle projekteja, erityisesti niille, jotka pyrkivät laajentamaan Sheets-, Docs-, Forms- ja muiden Google-sovellusten toiminnallisuutta.
