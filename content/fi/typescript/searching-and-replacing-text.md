---
title:    "TypeScript: Tekstin hakeminen ja korvaaminen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoinnissa, haluat korvata tietyt merkkijonot tai ilmaisut toisilla. Tämä voi olla tarpeellista esimerkiksi silloin, kun haluat muuttaa vanhoja koodinpätkiä uuden standardin mukaiseksi tai korjata kirjoitusvirheet. Tämän vuoksi on tärkeää tietää, miten tekstiä voi etsiä ja korvata TypeScriptiä käyttäen.

## Kuinka

Käytössäsi on useita tapoja etsiä ja korvata merkkijonoja TypeScriptissä. Yksinkertaisin tapa on käyttää sisäänrakennettua metodia ```replace```. Tämä metodi ottaa kaksi parametria: ensimmäinen on merkkijono, jota haluat etsiä ja toinen on merkkijono, jolla haluat korvata löydetyt merkkijonot. Alla esimerkki koodinpätkästä ja sen tulosteen:

```TypeScript
let text = "Tervetuloa maailmaan!";
let newText = text.replace("Tervetuloa", "Hei");
console.log(newText); // "Hei maailmaan!"
```

Jos haluat etsiä ja korvata useita merkkijonoja, voit käyttää Regexiä eli säännöllisiä lausekkeita. Käyttämällä Regexiä, voit huomioida esimerkiksi kirjainkoosta riippumatta löytyvät merkkijonot. Alla esimerkki koodinpätkästä ja sen tulosteen:

```TypeScript
let text = "Tervetuloa maailmaan!";
let newText = text.replace(/maailmaan/gi, "tulevaisuuteen");
console.log(newText); // "Tervetuloa tulevaisuuteen!"
```

## Syvärämaan syövereissä

Erilaisia tapoja etsiä ja korvata tekstiä on useita, ja ne kaikki tarjoavat erilaisia mahdollisuuksia ja ominaisuuksia. Voit esimerkiksi käyttää ```split```- ja ```join```-metodeja yhdessä, jotta voit muokata tekstiä ennen kuin korvaat sen. Voit myös käyttää Regexin lisäksi muita sisäänrakennettuja metodeja, kuten ```search```, joka palauttaa merkkijonon indeksin, jos se löytyy tekstistä.

On myös hyvä muistaa, että hakutulosten mukana ei välttämättä tule tietoa siitä, kuinka monta kertaa merkkijono löytyi ja korvattiin. Siksi on tärkeää hyödyntää erilaisia metodeja ja työkaluja, jotta voit varmistaa, että korvaat kaikki haluamasi merkkijonot.

## Katso myös

- [MDN web docs: String replace method (englanniksi)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN web docs: Regular Expressions (englanniksi)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [StackOverflow: How to replace all occurrences of a string in JavaScript (englanniksi)](https://stackoverflow.com/questions/1144783/how-to-replace-all-occurrences-of-a-string-in-javascript)