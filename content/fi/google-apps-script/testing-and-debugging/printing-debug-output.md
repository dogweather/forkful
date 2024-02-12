---
title:                "Debug-tulosteen tulostaminen"
aliases: - /fi/google-apps-script/printing-debug-output.md
date:                  2024-02-01T21:57:51.986769-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debug-tulosteen tulostaminen"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/google-apps-script/printing-debug-output.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Debug-tulosteen tulostaminen sisältää logilauseiden strategisen sijoittamisen koodiisi näyttämään muuttujien arvot, suoritusvirran tai virheviestit suoritusaikana. Ohjelmoijat käyttävät sitä laajasti seuratakseen ja diagnosoidakseen skriptiensä käyttäytymistä varmistaakseen niiden oikeellisuuden ja tehokkuuden Google Apps Script -sovelluksissaan.

## Kuinka:

Google Apps Script tarjoaa `Logger`-luokan perusdebuggaukseen ja kehittyneempiin tarpeisiin V8-ajoympäristössä esitellyn `console`-luokan.

**Loggerin käyttö:**

Logger-luokan avulla voit logittaa debug-viestejä, joita voit tarkastella suorituksen jälkeen Apps Script Editorissa kohdassa `Näkymä > Lokit`. Tässä on yksinkertainen esimerkki:

```javascript
function logSample() {
  var name = "Wired Reader";
  Logger.log("Hei, %s!", name);
}
```

Suoritettuasi `logSample()`, voit tarkastella lokia, jossa on "Hei, Wired Reader!" Lokinäkymässä.

**console.log-käyttö V8-ajoympäristössä:**

V8-ajoympäristössä `console.log` tarjoaa tutumman syntaksin kehittäjille, jotka tulevat muista kielistä:

```javascript
function consoleSample() {
  var status = 'aktiivinen';
  var count = 150;
  console.log(`Nykyinen tila: ${status}, Luku: ${count}`);
}
```

Suorituksen jälkeen voit käyttää Stackdriver-lokitusta kohdassa `Näkymä > Stackdriver-lokitus` tulosteen tarkasteluun. Se on voimakkaampi, tuki merkkijonojen interpoloinnille ja objektien tarkastelulle, ja integroituu Google Cloudin lokitukseen, tarjoten pysyvät lokit ja kehittyneet suodatuskyvyt.

**Näyte ulostulosta console.logista:**

```
Nykyinen tila: aktiivinen, Luku: 150
```

## Syväsukellus

Alun perin `Logger.log` oli ensisijainen työkalu debuggaukseen Google Apps Scriptissä tarjoten yksinkertaisen, suoraviivaisen tavan tulostaa tarkasteltavaa ulostuloa. Kuitenkin, kun skriptit muuttuivat monimutkaisemmiksi ja integroituneemmaksi Google Cloud Platform -palveluihin, selkeä tarve robustimmalle lokitusratkaisulle tuli ilmeiseksi.

Tässä tulee V8-ajoympäristö, tuoden `console.log` mukanaan. Tämä ei ainoastaan sovita Google Apps Scriptiä standardi JavaScript-syntaksiin, tehdäkseen kielen helpommin lähestyttäväksi kehittäjille, jotka ovat tuttuja JavaScriptin kanssa, vaan myös hyödyntää Google Cloudin lokituskyvykkyyksien voimakasta infrastruktuuria. `console.log`in esittely ja sen integrointi Google Cloud Platformiin merkitsee merkittävää kehitystä Google Apps Scriptin debuggauksessa, tarjoten kehittäjille dynaamisemman ja skaalautuvamman lähestymistavan skriptiensä seurantaan ja vianmääritykseen.

Vaikka `Logger.log` riittää perusdebuggaustarpeisiin ja pieniin projekteihin, `console.log` V8-ajoympäristössä tarjoaa kattavamman ja tulevaisuudenkestävämmän ratkaisun. Tämä sisältää kyvyn säilyttää lokit suoritusistunnon yli, etsiä ja suodattaa lokeja Google Cloud -konsolissa ja yleinen linjaus modernien JavaScript-kehityskäytäntöjen kanssa. Kehittäjien tulisi kuitenkin arvioida tarpeitaan projektinsa monimutkaisuuden ja laajuuden kannalta valitessaan näiden vaihtoehtojen välillä.
