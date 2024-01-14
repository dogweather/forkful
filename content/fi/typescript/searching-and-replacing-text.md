---
title:                "TypeScript: Tekstin etsiminen ja korvaaminen"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi
Jokainen ohjelmoija törmää joskus ongelmaan, jossa halutaan vaihtaa tietyt merkkijonot toisiksi. Tämä voi johtua esimerkiksi tarpeesta korjata kirjoitusvirheitä tai vaihtaa vanhentuneita termejä uusiin. Onneksi TypeScript tarjoaa helpon tavan tehdä nämä muutokset helposti ja nopeasti.

## Näin teet sen
```TypeScript
// Luodaan muuttuja, jossa on alkuperäinen merkkijono
const alkuperainenMerkkijono = "Tervetuloa TypeScriptin maailmaan!"

// Käytetään replace-metodia vaihtamaan "TypeScriptin" "ohjelmoinnin" todeksi
const uusiMerkkijono = alkuperainenMerkkijono.replace("TypeScriptin", "ohjelmoinnin");

// Tulostetaan uusi merkkijono konsoliin
console.log(uusiMerkkijono);

// Output: Tervetuloa ohjelmoinnin maailmaan!
```

Koodiesimerkissä käytämme replace-metodia vaihtamaan "TypeScriptin" "ohjelmoinnin" todeksi. Metodi ottaa vastaan kaksi parametria, ensimmäisenä oleva merkkijono, jota halutaan muuttaa, ja toisena oleva uusi merkkijono, joka korvaa alkuperäisen. Tuloksena saamme uuden merkkijonon, jossa alkuperäinen merkkijono on muutettu halutulla tavalla. Tämä metodi toimii myös silloin, kun halutaan vaihtaa useita merkkijonoja kerralla.

## Syvemmälle aiheeseen
Replace-metodi on vain yksi esimerkki siitä, kuinka TypeScript tarjoaa helpon ja tehokkaan tavan tehdä tekstikorvauksia. Lisäksi voimme käyttää esimerkiksi regular expressioneita (regex), joiden avulla voimme määrittää tarkemmin, mitä merkkijonoja haluamme vaihtaa. Regexien käyttäminen vaatii hieman enemmän oppimista, mutta ne tarjoavat suuremman joustavuuden ja tarkkuuden tekstin muokkaamisessa.

## Katso myös
- [TypeScript virallinen dokumentaatio](https://www.typescriptlang.org/docs/)
- [Regular expression -opas](https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Crash Course -kurssi](https://www.youtube.com/watch?v=rAy_3SIqT-E)