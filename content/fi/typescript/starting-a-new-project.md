---
title:    "TypeScript: Uuden projektin aloittaminen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# Miksi aloittaa uusi projekti?

On monia syitä, miksi kannattaa aloittaa uusi TypeScript-projekti. Jotkut saattavat olla innokkaita kokeilemaan uusia teknologioita ja päivittämään taitojaan, kun taas toisilla voi olla tarve luoda tehokkaampi ja ylläpidettävämpi ohjelmisto. Yksi suurimmista eduista on TypeScriptin tarjoama tyyppiturva, joka auttaa vähentämään virheitä ja parantamaan koodin laatua.

## Kuinka aloittaa TypeScript-projekti?

### Asenna TypeScript

Ensimmäinen askel on asentaa TypeScript-kielen komentorivityökalut ohjeiden mukaan. Voit tehdä tämän komennolla ```npm install -g typescript```. Tämän jälkeen voit tarkistaa asennuksen onnistumisen komennolla ```tsc -v```, joka tulisi näyttää asennetun TypeScript-version.

### Luo TypeScript-projekti

Seuraavaksi sinun täytyy luoda uusi kansio projektillesi ja siirtyä siihen komentokehotteessa. Voit sitten luoda ```package.json``` -tiedoston komennolla ```npm init```. Tämän jälkeen voit asentaa tarvittavat riippuvuudet komennolla ```npm install --save-dev typescript @types/node```, joka asentaa TypeScriptin ja tarvittavat tyypit Node.js:lle.

### Kirjoita koodia

Nyt olet valmis kirjoittamaan TypeScript-koodia! Luo uusi tiedosto ```.ts``` -päätteellä ja kirjoita haluamasi koodi. Muista käyttää tyyppien annotointia hyödyntääksesi TypeScriptin tarjoamia etuja. Kun olet valmis, voit suorittaa koodisi komennolla ```tsc tiedostonimi.ts```, joka muuntaa koodisi JavaScriptiksi.

## Syvällisempi sukellus uuden projektin aloittamiseen

Kun olet luonut uuden TypeScript-projektin, on tärkeää muistaa muutamia asioita sen ylläpidon helpottamiseksi. Ensinnäkin, hyödynnä tyyppien annotointia niin paljon kuin mahdollista - se auttaa vähentämään virheitä ja tekee koodista helpommin ylläpidettävän. Lisäksi, kannattaa määrittää ```tsconfig.json``` -tiedosto, joka määrittelee TypeScript-projektisi asetukset, kuten sallitut kääntämisen osajoukot ja kohdekansioiden määritykset.

Tärkein asia, jonka muistaa aloittaessa uutta projektia TypeScriptillä, on pitää avoin mieli ja olla valmis oppimaan uutta. Vaikka TypeScript voi vaikuttaa alussa vaikealta, sen tarjoamat edut tekevät siitä varteenotettavan vaihtoehdon JavaScriptille.

# Katso myös

- [TypeScript-projektin aloituksen opas](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- [TypeScriptin asentaminen](https://www.typescriptlang.org/download)
- [TypeScript ja Node.js -projektin alustus](https://dev.to/aurelkurtula/getting-started-with-typescript-in-nodejs-1kbb)