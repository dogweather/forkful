---
title:    "TypeScript: Merkkijonon pituuden löytäminen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa tarvitsemme selvittää merkkijonon pituuden TypeScriptissä. Tämä taito on tärkeä ohjelmoinnin perustaito, joka auttaa meitä käsittelemään ja käyttämään merkkijonoja monipuolisesti.

## Miten

```TypeScript
// Luodaan muuttuja, joka sisältää merkkijonon
const merkkijono: string = "Tämä on esimerkkimerkkijono";

// Käytetään sisäänrakennettua .length -metodia, joka palauttaa merkkijonon pituuden
console.log(merkkijono.length); // Output: 27
```

Kuten näemme yllä olevassa koodiesimerkissä, merkkijonon pituus saadaan käyttämällä merkkijonon nimeä, pisteellä erotettuna ja siihen liitettyä .length -metodia. Metodi palauttaa kokonaislukuna merkkijonon merkkien lukumäärän.

## Syvällisemmin

Merkkijonon pituuden laskeminen tapahtuu käyttämällä .length -metodia, joka on sisäänrakennettu metodi TypeScriptissä. Tämä metodi toimii kaikille merkkijonoille ja se palauttaa aina kokonaislukuna merkkijonon merkkien lukumäärän. Metodin käyttäminen on nopea ja helppo tapa selvittää merkkijonon pituus.

On myös hyvä huomata, että merkkijonon pituuteen lasketaan mukaan myös välilyönnit ja erikoismerkit. Esimerkiksi merkkijono "Hello world!" pituus on 12, vaikka siinä on vain 11 näkyvää merkkiä.

## Katso myös

- [TypeScriptin sisäänrakennetut metodit](https://www.typescriptlang.org/docs/handbook/strings.html#built-in-string-methods)
- [Merkkijonon ominaisuudet ja metodit TypeScriptissä](https://www.digitalocean.com/community/tutorials/how-to-work-with-strings-in-typescript#string-properties-and-methods-in-typescript)