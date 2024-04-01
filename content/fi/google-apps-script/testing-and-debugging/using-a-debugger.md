---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:35.319760-07:00
description: "Google Apps Scriptin (GAS) virheenkorjaus k\xE4sitt\xE4\xE4 prosessin,\
  \ jossa skripteist\xE4 tunnistetaan ja poistetaan virheit\xE4. N\xE4m\xE4 skriptit\
  \ on tarkoitettu Google\u2026"
lastmod: '2024-03-13T22:44:56.102273-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Scriptin (GAS) virheenkorjaus k\xE4sitt\xE4\xE4 prosessin, jossa\
  \ skripteist\xE4 tunnistetaan ja poistetaan virheit\xE4. N\xE4m\xE4 skriptit on\
  \ tarkoitettu Google\u2026"
title: "Debuggerin k\xE4ytt\xF6"
---

## Kuinka:
Google Apps Script tarjoaa sisäänrakennetun virheenkorjaustyökalun Apps Script -editorissa skriptien vianmääritystä varten. Näin voit aloittaa ja käyttää virheenkorjaustyökalua:

1. **Avaa skriptisi Apps Script -editorissa.**
2. **Valitse debuggattava funktio.** Avattavasta valikosta ylhäällä, valitse funktio, jota haluat debugata.
3. **Aseta keskeytyspisteitä.** Klikkaa reunaa (harmaa alue rivinumeroiden vasemmalla puolella) siinä kohdassa, jossa haluat keskeyttää suorituksen; punainen piste ilmestyy, osoittaen keskeytyspistettä.
4. **Aloita virheenkorjaus.** Klikkaa virheenkorjausikonia tai valitse `Debug` > `Start debugging`. Suoritus alkaa ja keskeytyy ensimmäisessä keskeytyspisteessä.

Harkitse tätä yksinkertaista skriptiä:

```javascript
function calculateSum() {
  var a = 5;
  var b = 10;
  var sum = a + b;
  Logger.log(sum); // Tarkoituksena kirjata 15
}
```

Jos et ole varma, miksi `Logger.log(sum)` ei näytä odotettua tulosta, voisit asettaa keskeytyspisteen riville `var sum = a + b;` ja kulkea läpi skriptin rivi riviltä tarkastaaksesi muuttujien arvot.

**Esimerkkilokituloste Loggerissa:**

```plain
15
```

Virheenkorjauksen aikana Apps Script -editori mahdollistaa sinun:

- **Käydä läpi koodia** käyttäen askel yli, askel sisään ja askel ulos -painikkeita.
- **Tarkkailla lausekkeita ja muuttujia** nähdäksesi niiden arvojen muuttuvan reaaliajassa.
- **Tutkia kutsupinoa** jäljittääksesi funktiokutsuja.

## Syväsukellus
Virheenkorjaus Google Apps Scriptissä, kuten missä tahansa muussa ohjelmointiympäristössä, on oleellista virheettömien sovellusten luomiselle. GAS:n kehityksen alkuvaiheessa esitelty sisäänrakennettu virheenkorjaustyökalu tarjoaa perusvalmiudet koodin tarkasteluun ja korjaamiseen vaiheittain. Vaikka se tarjoaa perusominaisuuksia, jotka ovat samankaltaisia kuin kypsemmistä ympäristöistä, kuten Visual Studio Code tai IntelliJ, löytyvät ominaisuudet, se saattaa jäädä puutteelliseksi monimutkaisissa virheenkorjaustilanteissa. Esimerkiksi sen kyvyt tarkastella asynkronisia takaisinkutsuja tai käsitellä raskaita skriptisuorituksia voivat olla rajoittuneet.

Monimutkaisiin virheenkorjaustarpeisiin kehittäjät saattavat turvautua vaihtoehtoisiin menetelmiin, kuten laajoihin lokitustöihin (käyttäen `Logger.log()`) tai jopa web-sovelluksen julkaisemiseen todellisen maailman skenaarion tarkasteluun. GAS:n virheenkorjaustyökalun yksinkertaisuus ja integraatio Apps Script -editoriin tekevät siitä kuitenkin arvokkaan ensiaskelen skriptien käyttäytymisen vianmäärityksessä ja ymmärtämisessä. Merkittävästi Google jatkaa Apps Scriptin päivityksiä ja parannuksia, mikä tasaisesti parantaa virheenkorjauskokemusta tarjoten ajan mittaan yhä kehittyneempiä työkaluja ja vaihtoehtoja. Tämä kehitys heijastaa Googlen sitoutumista tekemään Apps Scriptistä entistä tehokkaamman ja saavutettavamman alustan monen taustan kehittäjille.
