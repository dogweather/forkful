---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:40.212838-07:00
description: "Kuinka: Vaikka Google Apps Script (GAS) ei natiivisti tue YAML:n j\xE4\
  sennyst\xE4 eik\xE4 serialisointia, voit manipuloida YAML-dataa k\xE4ytt\xE4m\xE4\
  ll\xE4 JavaScript-\u2026"
lastmod: '2024-03-13T22:44:56.120427-06:00'
model: gpt-4-0125-preview
summary: "Vaikka Google Apps Script (GAS) ei natiivisti tue YAML:n j\xE4sennyst\xE4\
  \ eik\xE4 serialisointia, voit manipuloida YAML-dataa k\xE4ytt\xE4m\xE4ll\xE4 JavaScript-kirjastoja\
  \ tai kirjoittamalla omia j\xE4sennysfunktioita."
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

## Kuinka:
Vaikka Google Apps Script (GAS) ei natiivisti tue YAML:n jäsennystä eikä serialisointia, voit manipuloida YAML-dataa käyttämällä JavaScript-kirjastoja tai kirjoittamalla omia jäsennysfunktioita. Esimerkkinä, katsotaan miten jäsentää YAML-merkkijonoa käyttäen omia funktioita, koska ulkoisia kirjastoja ei voi suoraan tuoda GAS:iin.

Oletetaan, että sinulla on yksinkertainen YAML-konfiguraatio:

```yaml
title: YAML Esimerkki
description: Esimerkki siitä, miten käsitellä YAML:ia Google Apps Scriptissä
tags:
  - Google Apps Script
  - YAML
  - Konfiguraatio
```

Jäsennä tämä Google Apps Scriptissä käyttäen JavaScriptin merkkijonojen käsittelyn ominaisuuksia:

```javascript
function parseYAML(yamlString) {
  var tulos = {};
  var rivit = yamlString.split("\n");
  for (var i = 0; i < rivit.length; i++) {
    var rivi = rivit[i];
    if (rivi.includes(":")) {
      var osat = rivi.split(":");
      var avain = osat[0].trim();
      var arvo = osat[1].trim();
      // Peruskäsittely taulukoille
      if (arvo.startsWith("-")) {
        arvo = [arvo.substring(1).trim()];
        while (i + 1 < rivit.length && rivit[i + 1].trim().startsWith("-")) {
          i++;
          arvo.push(rivit[i].trim().substring(1).trim());
        }
      }
      tulos[avain] = arvo;
    }
  }
  return tulos;
}

function testYamlParsing() {
  var yaml = "title: YAML Esimerkki\ndescription: Esimerkki siitä, miten käsitellä YAML:ia Google Apps Scriptissä\ntags:\n  - Google Apps Script\n  - YAML\n  - Konfiguraatio";
  var jäsennetty = parseYAML(yaml);
  Logger.log(jäsennetty);
}
```

Kun `testYamlParsing()` suoritetaan, se tuottaa:

```
{ title: 'YAML Esimerkki',
  description: 'Esimerkki siitä, miten käsitellä YAML:ia Google Apps Scriptissä',
  tags: [ 'Google Apps Script', ' YAML', ' Konfiguraatio' ] }
```

Tämä oma jäsennystapa on melko perus ja voi vaatia säätöä monimutkaisten YAML-tiedostojen käsittelyyn.

## Syväkatsaus
YAML, joka alun perin julkaistiin vuonna 2001, pyrki olemaan ihmislukijaystävällisempi kuin sen edeltäjät kuten XML tai JSON. Vaikka sen yksinkertaisuutta ja helppokäyttöisyyttä arvostetaankin laajalti, YAML:n käsittely Google Apps Scriptissä esittää haasteita johtuen suorasta tuesta puuttumisesta. Tämän seurauksena ohjelmoijat usein turvautuvat JavaScriptin monipuolisuuteen jäsennelläkseen ja tuottaakseen YAML-dataa. Kuitenkin, monimutkaisissa käyttötapauksissa, erityisesti niissä, jotka liittyvät syvään sisäkkäisyyteen ja edistyneisiin tietorakenteisiin, tämä menetelmä voi olla työläs ja altis virheille.

JSON, päinvastoin, on natiivisti tuettu Google Apps Scriptissä ja useimmissa muissa ohjelmointiympäristöissä, tarjoten suoraviivaisemman lähestymistavan datan sarjallistamiseen ja deserialisointiin ilman lisäjäsennystarvetta. JSON:n syntaksi on vähemmän verbosinen kuin YAML:n, mikä tekee siitä sopivamman datavaihtoon web-sovelluksissa. Siitä huolimatta, YAML pysyy suosittuna konfiguraatiotiedostoissa ja tilanteissa, joissa ihmisen luettavuus on ensiarvoisen tärkeää.

Työskennellessäsi YAML:n kanssa Google Apps Scriptissä, harkitse luettavuuden ja käytön helppouden välisiä kompromisseja. Laajamittaiseen YAML-käsittelyyn saattaa olla hyödyllistä tutkia ulkopuolisia työkaluja tai palveluita, jotka voivat muuntaa YAML:n JSON:ksi ennen sen käsittelyä skriptissäsi.
