---
date: 2024-01-20 18:01:16.958881-07:00
description: "How to: - Kuinka tehd\xE4: L\xE4hett\xE4m\xE4ll\xE4 HTTP-pyynt\xF6j\xE4\
  \ ohjelmat voivat vaikuttaa tietoihin toisella tietokoneella, yleens\xE4 palvelimella.\
  \ K\xE4yt\xE4nt\xF6 alkoi\u2026"
lastmod: '2024-04-05T22:51:10.462244-06:00'
model: gpt-4-1106-preview
summary: "- Kuinka tehd\xE4: L\xE4hett\xE4m\xE4ll\xE4 HTTP-pyynt\xF6j\xE4 ohjelmat\
  \ voivat vaikuttaa tietoihin toisella tietokoneella, yleens\xE4 palvelimella. K\xE4\
  yt\xE4nt\xF6 alkoi 1990-luvulla ja on nyt vakiintunut kehitysmalli verkkopalveluissa.\
  \ Vaihtoehtoja `axios`-kirjastolle ovat muun muassa `fetch`-API, joka on sis\xE4\
  \xE4nrakennettu selaimiin, ja `http`-moduuli Node.js:ss\xE4. Axios on valittu sen\
  \ helppok\xE4ytt\xF6isyyden ja lupausten (promises) tukemisen vuoksi. L\xE4hett\xE4\
  ess\xE4si pyynt\xF6j\xE4 on t\xE4rke\xE4\xE4 hallita asynkronisuus. TypeScript ja\
  \ moderni JavaScript tarjoavat `async/await` syntaksin, joka tekee prosessista selke\xE4\
  mm\xE4n. T\xE4m\xE4n avulla voidaan kirjoittaa koodia, joka odottaa vastausta ennen\
  \ kuin jatkaa."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

## How to: - Kuinka tehdä:
```TypeScript
import axios from 'axios'; // HTTP-kirjastoa varten

// Asynkroninen funktio lähetä GET-pyyntö
async function fetchSomeData() {
  try {
    const response = await axios.get('https://api.example.com/data');
    console.log(response.data); // Käsittelee vastaanotetut tiedot
  } catch (error) {
    console.error(error); // Hallitsee virhetilanteita
  }
}

// Kutsu funktiota
fetchSomeData();
```

```TypeScript
// Asynkroninen funktio lähetä POST-pyyntö ja lähetä dataa
async function sendSomeData(someData: any) {
  try {
    const response = await axios.post('https://api.example.com/submit', someData);
    console.log(response.status); // Vastausstatuksen tarkistus
  } catch (error) {
    console.error(error);
  }
}

// Kutsu funktiota esimerkkidatalla
sendSomeData({ name: 'Tarmo', age: 28 });
```

## Deep Dive - Syväsukellus:
Lähettämällä HTTP-pyyntöjä ohjelmat voivat vaikuttaa tietoihin toisella tietokoneella, yleensä palvelimella. Käytäntö alkoi 1990-luvulla ja on nyt vakiintunut kehitysmalli verkkopalveluissa.

Vaihtoehtoja `axios`-kirjastolle ovat muun muassa `fetch`-API, joka on sisäänrakennettu selaimiin, ja `http`-moduuli Node.js:ssä. Axios on valittu sen helppokäyttöisyyden ja lupausten (promises) tukemisen vuoksi.

Lähettäessäsi pyyntöjä on tärkeää hallita asynkronisuus. TypeScript ja moderni JavaScript tarjoavat `async/await` syntaksin, joka tekee prosessista selkeämmän. Tämän avulla voidaan kirjoittaa koodia, joka odottaa vastausta ennen kuin jatkaa.

## See Also - Katso Myös:
- Axios dokumentaatio: [https://axios-http.com/docs/intro](https://axios-http.com/docs/intro)
- MDN Web Docs `fetch`: [https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- TypeScript Handbook: [https://www.typescriptlang.org/docs/handbook/intro.html](https://www.typescriptlang.org/docs/handbook/intro.html)
