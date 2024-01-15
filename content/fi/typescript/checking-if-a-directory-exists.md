---
title:                "Tarkistetaan onko kansiossa olemassa"
html_title:           "TypeScript: Tarkistetaan onko kansiossa olemassa"
simple_title:         "Tarkistetaan onko kansiossa olemassa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmointia tehdessä tarvitaan tarkistaa, onko tietyssä paikassa olemassa olevaa kansiorakennetta. Tämä voi olla hyödyllistä esimerkiksi tiettyjen tiedostojen käsittelyssä tai ohjelman toiminnan testaamisessa.

## Kuinka tehdä

```typescript
if (fs.existsSync("/polku/kansioon")) {
  console.log("Kansio on olemassa!");
} else {
  console.log("Kansiota ei ole olemassa!");
}
```

Yllä olevassa koodiesimerkissä käytetään TypeScriptin `fs`-moduulia tarkistamaan, onko annetussa polussa olevaa kansiorakennetta olemassa. Jos kansio löytyy, tulostetaan ilmoitus siitä. Muussa tapauksessa tulostetaan ilmoitus, että kansiota ei ole olemassa.

## Syventävä tieto

Tarkistaessasi kansioiden olemassaoloa kannattaa myös huomioida, että eri käyttöjärjestelmät voivat käyttää erilaisia polkumerkintöjä. Esimerkiksi Windows-käyttöjärjestelmässä polkumerkintä on yleensä muodossa `C:\polku\kansioon`, kun taas Unix-pohjaisissa järjestelmissä se voi olla muodossa `/polku/kansioon`.

Voit myös tarkistaa, onko kyseinen polku kansiorakenteeseen, jolloin `fs.existsSync()`-funktion paluuarvo on `false` jos polku osoittaa tiedostoon eikä kansiorakenteeseen.

## Katso myös

- [fs moduuli (Node.js)](https://nodejs.org/api/fs.html)
- [fs.existsSync() (Node.js)](https://nodejs.org/api/fs.html#fs_fsexistsync_path)