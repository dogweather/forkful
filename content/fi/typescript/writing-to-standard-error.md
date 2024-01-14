---
title:                "TypeScript: Kirjoittaminen standardivirheeseen"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Miksi kirjoittaa virheilmoituksiin TypeScript-ohjelmoinnissa?

On olemassa monia syitä, miksi kirjoittaminen virheilmoituksiin on hyödyllistä TypeScript-ohjelmoinnissa. Ensinnäkin, se auttaa havaitsemaan mahdolliset virheet koodissasi ja säästää aikaa etsiessäsi niitä myöhemmin. Lisäksi, se lisää koodisi luettavuutta ja helpottaa virheiden korjaamista.

## Miten se tehdään?

Kirjoittaminen virheilmoituksiin TypeScript-ohjelmoinnissa on helppoa ja siihen on monta tapaa. Yksi tapa on käyttää `console.error()` -funktiota, joka tulostaa haluamasi viestin konsoliin. Voit myös käyttää `process.stderr.write()` -funktiota, joka kirjoittaa virheilmoituksen suoraan standardivirhepuroon.

Esimerkki:

```TypeScript
try {
  // jokin koodi, joka saattaa aiheuttaa virheen
} catch (error) {
  // tulostaa virheilmoituksen konsoliin
  console.error("Tapahtui virhe:", error);
}
```

Tulostus:

```
Tapahtui virhe: Error: Tämä on virhe
```

Voit myös käyttää `console.trace()` -funktiota, joka tulostaa virheen lisäksi pinojäljen, helpottaen virheen syyn selvittämistä.

## Syventävä tieto

Virheilmoituksia voi myös muokata lisäämällä niihin lisätietoja, kuten ajankohdan, tiedoston tai rivinumeron. Tämä auttaa jäljittämään virheitä paremmin ja tekemään koodista luettavampaa.

Esimerkki:

```TypeScript
const filename = "tiedostonimi.ts";
const line = 5;
console.error(`${filename}, rivillä ${line}: Tapahtui virhe`);
```

Tulostus:

```
tiedostonimi.ts, rivillä 5: Tapahtui virhe
```

Muokatun virheilmoituksen lisäksi, voit myös tehdä siitä monikielisen käyttämällä `Intl`-rajapintaa ja `Intl.displayNames` -luokkaa. Tämä mahdollistaa virheilmoituksen näyttämisen eri kielillä riippuen käyttäjän asetuksista.

## Katso myös

- [TypeScript virheidenkäsittely](https://www.typescriptlang.org/docs/handbook/errors.html)
- [Node.js konsoliniin kirjoittaminen](https://nodejs.org/api/console.html)
- [MDN: Internationalisointi kansainvälisille sovelluksille](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl)