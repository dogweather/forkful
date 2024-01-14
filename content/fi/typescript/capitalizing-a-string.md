---
title:    "TypeScript: Pääkirjoittaminen merkkijonolle"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit käyttää aikaa ja vaivaa merkkijonon suurilla kirjaimilla kirjoittamiseen? Yksi yleinen syy on, että monissa ohjelmointikielessä merkkijonot kirjoitetaan normaalisti pienillä kirjaimilla. Näin ollen merkkijonon muuntaminen suuriksi kirjaimiksi on usein tarpeellista, jotta voidaan esimerkiksi verrata kahta merkkijonoa.

## Miten tehdä

### Käyttökelpoinen funktio
Yksi tapa tehdä tämä on luomalla oma funktio, joka hyväksyy merkkijonon parametrina ja palauttaa uuden merkkijonon, jossa kaikki kirjaimet on muutettu suuriksi kirjaimiksi. Tämä voidaan toteuttaa TypeScriptillä seuraavalla tavalla:

```TypeScript
function muutaSuuriksiKirjaimiksi(merkkijono: string): string {
    return merkkijono.toUpperCase();
}

// Kutsutaan funktiota ja tulostetaan palautettu arvo konsoliin
console.log(muutaSuuriksiKirjaimiksi("moi maailma")); // "MOI MAAILMA"
```

### Käyttö sisäänrakennetulla toiminnolla
Toinen vaihtoehto on käyttää TypeScriptin sisäänrakennettua toimintoa `toUpperCase()`, joka tekee saman asian kuin edellinen funktio. Alla on esimerkki tämän toiminnon käytöstä:

```TypeScript
// Luodaan uusi muuttuja, joka sisältää muutettavan merkkijonon
let teksti = "hei kaikille";

// Muutetaan merkkijonon kirjaimet suuriksi ja tallennetaan uuteen muuttujaan
let uusiTeksti = teksti.toUpperCase();

// Tulostetaan uusi merkkijono konsoliin
console.log(uusiTeksti); // "HEI KAIKILLE"
```

## Syvällisempi sukellus
Molemmat edellä mainitut vaihtoehdot ovat loistavia tapoja saada merkkijono kirjoitettua suurilla kirjaimilla. On kuitenkin huomattava, että TypeScriptin sisäänrakennettu toiminto `toUpperCase()` ei toimi kaikilla kielillä, joten jos käytät esimerkiksi ääkkösiä, tämä toiminto ei muuta niitä suuriksi kirjaimiksi. Tässä tapauksessa kannattaa luoda oma funktio, joka ottaa huomioon myös ääkköset.

Lisäksi, jos tarvitset muuttaa vain tietyn osan merkkijonosta suuriksi kirjaimiksi, voit käyttää TypeScriptin `substring()` -toimintoa. Tämä toiminto leikkaa ja palauttaa merkkijonosta halutun alueen, jonka jälkeen voit muuttaa tämän alueen suuriksi kirjaimiksi. Esimerkiksi:

```TypeScript
let teksti = "tervetuloa vieraille";
let haluttuOsa = teksti.substring(0, 10); // "tervetuloa"

// Muutetaan haluttu osa suuriksi kirjaimiksi ja yhdistetään se loppuosaan
let uusiTeksti = haluttuOsa.toUpperCase() + teksti.substring(10);

console.log(uusiTeksti); // "TERVETULOA vieraille"
```

## Katso myös

- [TypeScriptin virallinen verkkosivusto](https://www.typescriptlang.org/)
- [TypeScriptin dokumentaatio](https://www.typescriptlang.org/docs/)
- [TypeScriptin opetusohjelmat](https://www.typescriptlang.org/docs/home.html#tutorials)