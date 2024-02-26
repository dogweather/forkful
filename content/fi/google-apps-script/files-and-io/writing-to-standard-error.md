---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:04.414550-07:00
description: "Standardivirheen (stderr) kirjoittaminen ohjelmointikieliss\xE4 tarkoittaa\
  \ virheilmoitusten ja diagnostiikkojen ohjaamista erilliseen virtaan, pois\u2026"
lastmod: '2024-02-25T18:49:53.092288-07:00'
model: gpt-4-0125-preview
summary: "Standardivirheen (stderr) kirjoittaminen ohjelmointikieliss\xE4 tarkoittaa\
  \ virheilmoitusten ja diagnostiikkojen ohjaamista erilliseen virtaan, pois\u2026"
title: Kirjoittaminen vakiovirheeseen
---

{{< edit_this_page >}}

## Mikä & Miksi?

Standardivirheen (stderr) kirjoittaminen ohjelmointikielissä tarkoittaa virheilmoitusten ja diagnostiikkojen ohjaamista erilliseen virtaan, pois standarditulosteesta (stdout). Ohjelmoijat tekevät näin erottaakseen normaalin ohjelman tulosteen virheilmoituksista, mikä tekee vianetsinnästä ja lokianalyysistä suoraviivaisempaa.

## Kuinka:

Google Apps Script, ollessaan kevyiden sovellusten kehitykseen tarkoitettu skriptikieli Google Apps -alustalle, ei tarjoa suoraa sisäänrakennettua funktiota, kuten `console.error()` virheiden kirjoittamiseen stderr:iin, kuten saatat löytää Node.js:stä tai Pythonista. Voit kuitenkin simuloida tätä käyttäytymistä käyttämällä Google Apps Scriptin lokipalveluita tai mukautettua virheenkäsittelyä virhetulosteiden hallitsemiseen ja erotteluun.

### Esimerkki: Käytä `Logger`ia virheviestien kirjaamiseen

```javascript
function logError() {
  try {
    // Simuloi virhe
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("Yritetty jakaminen nollalla");
  } catch (e) {
    // Kirjoita virheviesti lokeihin
    Logger.log('Virhe: ' + e.message);
  }
}
```

Kun ajat `logError()`, tämä kirjoittaa virheviestin Google Apps Scriptin lokiin, jonka voit tarkastella valitsemalla `Näkymä > Lokit`. Tämä ei ole täsmälleen stderr, mutta se palvelee samanlaista tarkoitusta erottaen virhelokit tavallisista tulosteista.

### Edistynyt diagnostinen lokitus

Edistyneempään vianetsintään ja virhelokuun voit käyttää Stackdriver Loggingia, joka tunnetaan nykyisin nimellä Google Cloudin Operations Suite.

```javascript
function advancedErrorLogging() {
  try {
    // Aiheuta virhe tahallisesti
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('Kohdattu virhe: ', e.toString());
  }
}
```

Tämä ohjaa virheviestin Stackdriver Loggingiin, jossa sitä käsitellään virhetason lokina. Huomaa, että Stackdriver/Google Cloudin Operations Suite -integraatio tarjoaa tarkemman ja etsittävissä olevan lokiratkaisun verrattuna `Logger`iin.

## Syväsukellus

Dedikoidun `stderr`-virran puute Google Apps Scriptissä heijastaa sen luonnetta ja alkuperää pilvipohjaisena skriptikielenä, missä perinteiset konsoli- tai terminaalipohjaiset tulostukset (kuten stdout ja stderr) ovat vähemmän merkityksellisiä. Historiallisesti Google Apps Script suunniteltiin parantamaan Google Apps -toiminnallisuuksia yksinkertaisilla skripteillä, keskittyen käytön helppouteen kattavien ominaisuuksien sijaan, joita on saatavilla monimutkaisemmissa ohjelmointiympäristöissä.

Sanottuani, Google Apps Scriptin kehitys kohti monimutkaisempaa sovelluskehitystä on kannustanut kehittäjiä omaksumaan luovia lähestymistapoja virheenkäsittelyyn ja lokitukseen, hyödyntämällä saatavilla olevia palveluita kuten Logger ja integroitumalla Google Cloudin Operations Suiteen. Nämä menetelmät, vaikka eivät olekaan suoria stderr-toteutuksia, tarjoavat vankat vaihtoehdot virhehallintaan ja diagnostiseen lokitukseen pilvikeskeisessä ympäristössä.

Kriittisesti, kun nämä menetelmät palvelevat tarkoitustaan Google Apps Scriptin ekosysteemissä, ne korostavat alustan rajoituksia verrattuna perinteisiin ohjelmointiympäristöihin. Kehittäjille, jotka tarvitsevat yksityiskohtaisia ja hierarkkisia virheenkäsittelystrategioita, integroituminen ulkoisiin lokipalveluihin tai Google Cloud Functionsin käyttöönotto, jotka tarjoavat perinteisemmän stderr- ja stdout-käsittelyn, saattavat olla suositeltavampia.
