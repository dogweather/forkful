---
title:    "TypeScript: Merkkijonon muuttaminen isoiksi kirjaimiksi"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi

On olemassa useita syitä, miksi voit haluta käyttää TypeScriptiä ohjelmoinnissa. Yksi tärkeimmistä on sen vahva tyyppijärjestelmä, joka auttaa välttämään virheitä koodin kirjoittamisessa ja parantaa koodin luettavuutta ja ymmärrettävyyttä. Tämä pätee myös merkkijonojen käsittelyyn, kuten esimerkiksi merkkijonon muuttaminen suuriksi kirjaimiksi. Se voi helpottaa koodin luomista ja vähentää mahdollisia virheitä, jolloin ohjelmointikokemus on sujuvampi.

## Miten

Merkkijonon muuttaminen suuriksi kirjaimiksi on yksinkertaista tehdä TypeScriptillä. Käytä vain metodia "toUpperCase()", joka muuttaa merkkijonon kaikki kirjaimet suuriksi kirjaimiksi. Tämän jälkeen voit käyttää uutta muokattua merkkijonoa ohjelmassasi. Alla on esimerkki:

```TypeScript
let merkkijono = "tervetuloa!";
console.log(merkkijono.toUpperCase());
```

Tuloste olisi: "TERVETULOA!"

## Syvemmälle

Vaikka "toUpperCase()" onkin helppo tapa muuttaa merkkijonon kirjaimet suuriksi, on hyvä olla tietoinen myös muista vaihtoehdoista. Esimerkiksi, jos haluat muuttaa vain ensimmäisen kirjaimen suureksi, voit käyttää metodia "charAt()" yhdistettynä "toUpperCase()" metodiin. Tämä antaa sinulle enemmän hallintaa siitä, minkä kirjaimen haluat muuttaa.

```TypeScript
let merkkijono = "tervetuloa!";
console.log(merkkijono.charAt(0).toUpperCase() + merkkijono.slice(1));
```

Tuloste olisi: "Tervetuloa!"

## Katso myös

- [MDN Web Docs: String toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [W3Schools: TypeScript Strings](https://www.w3schools.com/typescript/typescript_strings.asp)
- [TypeScript Documentation: String methods](https://www.typescriptlang.org/docs/handbook/declaration-files/do-s-and-don-ts.html#supported-string-methods)