---
title:    "TypeScript: Virheenjäljitystulosteen tulostaminen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi

Debuggaustulostusten tulostaminen on tärkeä osa koodaamista, koska se auttaa kehittäjiä ymmärtämään ohjelmien toimintaa ja löytämään ja ratkaisemaan mahdollisia virheitä.

## Kuinka tehdä

Eräs tapa tulostaa debuggaustulostuksia on käyttää TypeScriptin ```console.log()``` -funktiota. Esimerkiksi:

```TypeScript
let nimi = "Sara";
console.log("Hei " + nimi + "!"); // Tulostaa "Hei Sara!" konsoliin
```

Toinen tapa on käyttää TypeScriptin sisäänrakennettua ```debugger``` -komennon avulla. Tämä komento pysäyttää ohjelman suorituksen ja antaa kehittäjän tarkastella ohjelman sisäistä tilaa. Esimerkiksi:

```TypeScript
let numerot = [1, 2, 3];
debugger; // Pysäyttää ohjelman suorituksen ja antaa tarkastella numerot-muuttujan sisältöä
console.log(numerot); // Tulostaa numerot-muuttujan sisällön konsoliin
```

## Syvällisempi tarkastelu

Tulostaminen debuggausvaiheessa auttaa kehittäjää ymmärtämään ohjelman suorituksen vaiheita ja löytämään mahdollisia virheitä. Se on myös hyödyllinen tapa selittää ohjelman toimintaa muille kehittäjille. Tärkeää kuitenkin muistaa, että live-ympäristössä ei tulisi käyttää debuggaus tulostuksia, sillä ne voivat hidastaa ohjelman suoritusta.

## Katso myös

- [TypeScriptin virallinen dokumentaatio](https://www.typescriptlang.org/docs/home.html)
- [Debuggaus-implementaatioita TypeScriptille](https://github.com/Microsoft/TypeScript/wiki/Debugging-TypeScript)