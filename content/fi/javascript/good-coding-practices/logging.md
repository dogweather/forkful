---
title:                "Lokitus"
aliases:
- /fi/javascript/logging/
date:                  2024-01-26T01:07:41.670265-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lokitus"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/logging.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lokitus on lyhyesti sanottuna kuin päiväkirjan pitämistä sovelluksellesi - se tallentaa tapahtumia, virheitä ja muita merkittäviä toimia, jotka tapahtuvat ohjelmiston suorituksen aikana. Ohjelmoijat tekevät sitä ei ainoastaan ymmärtääkseen, mitä konepellin alla tapahtuu reaaliajassa, vaan myös historiatietojen saamiseksi, mikä on ratkaisevan tärkeää virheenkorjauksessa, auditoinnissa ja suorituskyvyn optimoinnissa.

## Kuinka:
Valmiiksi pakattuna JavaScript tarjoaa yksinkertaisen tavan kirjata viestejä konsoliin:

```javascript
console.log('Tämä lokitetaan konsoliin');

// Tulostus:
// Tämä lokitetaan konsoliin
```

Mutta tosielämän sovellukset vaativat enemmän kuin vain viestien tulostamista konsoliin. Kirjastoja kuten Winston tai Pino voidaan tuoda mukaan hallitsemaan lokeja tehokkaasti:

```javascript
// Käyttäen Winsonia edistyneeseen lokitukseen
const winston = require('winston');

const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'combined.log' })
  ],
});

logger.info('Hei, tämä on lokitustapahtuma Winstonilla');
// Tämä logi kirjoitetaan 'combined.log'-tiedostoon JSON-muodossa
```

Esimerkki `combined.log` tuloste:

```json
{"message":"Hei, tämä on lokitustapahtuma Winstonilla","level":"info"}
```

## Syväsukellus
Lokitus on ollut olennainen osa tietotekniikan alkuaikoja lähtien; järjestelmäoperaattorit tutkisivat loki-tiedostoja ymmärtääkseen järjestelmän suorituskykyä ja diagnosoidakseen ongelmia. Nykyaikaista kehitystyötä silmällä pitäen, olemme siirtyneet yksinkertaisista lokitiedostoista rakenteellisiin ja haettaviin lokinhallintajärjestelmiin.

Vaihtoehdot konsolin tai tiedostopohjaiselle lokitukselle JavaScriptissa sisältävät pilvipohjaisten logituspalvelujen, kuten Loggly, Datadog tai ELK Stack (Elasticsearch, Logstash, Kibana), käyttämisen, jotka voivat aggregoida lokeja useista lähteistä, tarjota visualisointityökaluja ja edistyneitä analytiikkaratkaisuja.

Lokituksen toteuttamisessa harkitse seuraavia seikkoja:
- **Yksityiskohtien Taso**: Sisältäen debug, info, varoitus, virhe ja kriittinen.
- **Suorituskyky**: Liiallinen lokitus voi vaikuttaa sovelluksen suorituskykyyn.
- **Turvallisuus**: Ole varovainen herkän tiedon lokittamisessa.
- **Muoto**: Rakenteelliset lokit (kuten JSON) tekevät lokien etsimisestä ja jäsentämisestä helpompaa.
- **Säilytyskäytännöt**: Vanhat lokit täytyy arkistoida tai poistaa tilan säästämiseksi.

Käytännöllinen lokitusstrategia määrittelee, mitä lokitetaan, mihin sitä lokitetaan ja kuinka kauan sitä säilytetään, tasapainottaen informatiivista näkemystä suorituskyvyn ja yksityisyysnäkökohtien kanssa.

## Katso Myös
Tutustu näihin resursseihin syvemmälle sukeltamiseen:
- [Winston GitHub Repository](https://github.com/winstonjs/winston): syvälliselle käytölle ja mukautetuille siirtovälineille.
- [Pino - Erittäin vähän ylikuormitusta aiheuttava Node.js-lokitus](https://github.com/pinojs/pino): kevyt lokitusratkaisu.
- [MDN Web Docs: Konsoli](https://developer.mozilla.org/en-US/docs/Web/API/Console): ydinselainpohjaiselle lokitusinfolle.
- [Elastic ELK Stack](https://www.elastic.co/what-is/elk-stack): voimakas kolmikko lokien hallintaan.
- [12 Factor App Lokitus](https://12factor.net/logs): parhaat käytännöt sovelluslokissa.
