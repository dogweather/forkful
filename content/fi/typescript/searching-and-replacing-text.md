---
title:    "TypeScript: Tekstin etsiminen ja korvaaminen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi 

On monia syitä miksi ohjelmoijat saattavat joutua suorittamaan tekstin etsimistä ja korvaamista omassa koodissaan. Se voi olla tarpeellista, jos tiettyä tietoa on kirjoitettu useisiin kohtiin ja sitä halutaan muuttaa kaikista samalla kertaa. Tämä auttaa myös välttämään manuaalista työtä, kun kyseessä on suuri kooditietokanta.

## Kuinka

Etsimisen ja korvaamisen toteuttaminen TypeScript-ohjelmointikielellä on helppoa. Voit käyttää String-tyypin replace-metodia ja tarvittaessa antaa sille RegExp-parametrin, joka määrittää etsittävän merkkijonon. Esimerkiksi, jos haluat korvata kaikki "Hello" merkkijonot "Hei" merkkijonoilla, voit käyttää seuraavaa koodia:

```TypeScript
let newText = "Hello world!".replace(/Hello/g, "Hei");
console.log(newText); // Output: "Hei world!"
```

RegExp-parametri "g" tarkoittaa kaikkia esiintymisiä ja siksi se korvaa kaikki "Hello" merkkijonot.

##Syvä sukellus

Regular expressions ovat erittäin hyödyllisiä tekstin etsimiseen ja korvaamiseen. Voit käyttää niitä tarkemmin määrittelemään etsittävän merkkijonon. Esimerkiksi, jos haluat korvata kaikki "Hello" merkkijonot, jotka ovat sanojen ja sulkumerkkien välissä, voit käyttää seuraavaa koodia:

```TypeScript
let newText = "Hello world! (Hello there)".replace(/\(Hello\)/g, "(Hei)");
console.log(newText); // Output: "Hello world! (Hei there)"
```

Esimerkissä käytetty RegExp-parametri tarkoittaa "Hello" sanaa sulkumerkkien välissä ja korvaa vain sen "Hei" sanalla.

[JavaScript RegExp-opas] (https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Regular_Expressions)

[RegExp Validator] (https://regex101.com/)

[TypeScript Stringin replace-metodi] (https://www.typescriptlang.org/docs/handbook/strings.html#the-replace-method)

## Katso myös

[TypeScriptin viralliset verkkosivut] (https://www.typescriptlang.org/)

[TypeScript-opetusohjelma] (https://www.tutorialspoint.com/typescript/index.htm)

[TypeScriptin GitHub-sivu] (https://github.com/Microsoft/TypeScript)