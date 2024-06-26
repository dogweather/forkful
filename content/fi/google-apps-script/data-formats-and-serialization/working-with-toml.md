---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:32.885558-07:00
description: "Kuinka: Koska Google Apps Script on k\xE4yt\xE4nn\xF6ss\xE4 JavaScriptill\xE4\
  \ toteutettu yhteys Googlen sovelluskokonaisuuteen, on TOML:n suora k\xE4ytt\xF6\
  \ Google Apps\u2026"
lastmod: '2024-03-13T22:44:56.123828-06:00'
model: gpt-4-0125-preview
summary: "Koska Google Apps Script on k\xE4yt\xE4nn\xF6ss\xE4 JavaScriptill\xE4 toteutettu\
  \ yhteys Googlen sovelluskokonaisuuteen, on TOML:n suora k\xE4ytt\xF6 Google Apps\
  \ Scriptiss\xE4 hieman oveluutta vaativaa."
title: "TOML:n kanssa ty\xF6skentely"
weight: 39
---

## Kuinka:
Koska Google Apps Script on käytännössä JavaScriptillä toteutettu yhteys Googlen sovelluskokonaisuuteen, on TOML:n suora käyttö Google Apps Scriptissä hieman oveluutta vaativaa. Google Apps Script ei natiivisti tue TOML-tulkintaa, mutta voit hyödyntää JavaScript-kirjastoja tai kirjoittaa yksinkertaisen tulkin perustarpeisiin.

Käsitellään esimerkkinä yksinkertainen TOML-konfiguraatiojono:

```javascript
// TOML-merkkijono
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// Yksinkertainen TOML JSON -tulkki
function parseTOML(tomlStr) {
  var tulos = {};
  var nykyinenOsa = tulos;
  tomlStr.split(/\r?\n/).forEach(rivi => {
    rivi = rivi.trim();
    if (rivi.startsWith('[')) { // Uusi osa
      var osanNimi = rivi.replace(/\[|\]/g, '');
      tulos[osanNimi] = {};
      nykyinenOsa = tulos[osanNimi];
    } else if (rivi) {
      var avainArvo = rivi.split('=').map(osa => osa.trim());
      var avain = avainArvo[0];
      var arvo = eval(avainArvo[1]); // Käytä evalia yksinkertaisuuden vuoksi; ole varovainen tuotantokoodissa
      nykyinenOsa[avain] = arvo;
    }
  });
  return tulos;
}

// Testaa tulkinta
var configObject = parseTOML(tomlString);
console.log(configObject);

```

Esimerkkilähtö `console.log`:sta muistuttaisi JSON-objektia, jolloin konfiguraatio-ominaisuuksiin pääseminen Google Apps Scriptissä olisi helpompaa:

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## Syväsykellus
TOML:n loi Tom Preston-Werner, yksi GitHubin perustajista, olemaan ihmisläheisempi kuin JSON konfiguraatiotiedostoille säilyttäen samalla kyvyn jäsentää se yksiselitteisesti. Se pyrkii olemaan mahdollisimman yksinkertainen, tavoite, joka sopii hyvin monien kehitysprojektien eetokseen, jotka pyrkivät yksinkertaisuuteen ja luettavuuteen koodikannoissaan.

Google Apps Scriptin kontekstissa TOML:n käyttö voi tuoda mukanaan joitakin haasteita, ottaen huomioon suoran tuen puutteen ja tarpeen analysoida se manuaalisesti tai kolmansien osapuolten kirjastojen kautta. Pienemmille projekteille tai niille, jotka eivät ole syvällisesti integroituneita Googlen ekosysteemiin, vaihtoehdot, kuten JSON tai jopa yksinkertaiset avain-arvo-parirakenteet skriptiominaisuuksissa, voivat riittää ja olla suoraviivaisempia toteuttaa. Kuitenkin sovelluksille, jotka painottavat ihmisläheisiä konfiguraatiotiedostoja ja ovat jo sitoutuneet TOML:ään, TOML-tulkkauksen integrointi mukautettujen skriptien kautta lisää käyttökelpoista joustavuutta ja ylläpidettävyyttä poikkeamatta valitusta konfiguraatioparadigmasta.
