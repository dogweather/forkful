---
title:    "TypeScript: Aloittamassa uutta projektia"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Aloittaessasi uuden projektin TypeScriptilla, pystyt helpommin kirjoittamaan ja ylläpitämään suurta tietokoodipohjaa. TypeScript tarjoaa parempaa kirjoitus- ja virheenkorjaustyökaluja verrattuna perinteisiin Javascript-kieleen.

## Kuinka aloittaa

Aloita asentamalla TypeScript-compiler NPM:n kautta. Seurauksen koodinpaloista näet miten TypeScript-koodi näyttää tavallisesti. 

```TypeScript
npm install -g typescript
```

Seuraavaksi voit luoda uuden TypeScript-projektin nimeltä "example".

```TypeScript
mkdir example
cd example
```

Luo uusi TypeScript-tiedosto nimeltä "app.ts".

```TypeScript
touch app.ts
```

Kirjoita koodisi "app.ts" tiedostossasi.

```TypeScript
function greeting(name: string) {
  console.log("Hei " + name + ", tervetuloa TypeScript-maailmaan!");
}

let name = "Finnish readers";
greeting(name);
```

Käännä koodisi TypeScriptista Javascriptiksi suorittamalla komento `tsc app.ts`. Tämä generoi uuden Javascript-tiedoston nimeltä "app.js".

Voit nyt ajaa Javascript-tiedostosi komentoriviltä käyttämällä komentoa `node app.js`. Näet seuraavanlaisen tulosteen:

```TypeScript
Hei Finnish readers, tervetuloa TypeScript-maailmaan!
```

## Syväsukellus

Jos haluat aloittaa suuremman TypeScript-projektin, voit harkita Typescriptin käyttämistä yhdessä monien suosittujen kehitysympäristöjen kanssa, kuten Visual Studio Code, WebStorm, Atom tai Sublime Text. Nämä kehitysympäristöt tarjoavat lisäominaisuuksia, kuten automaattisen koodin korjaamisen ja debuggerin, jotka voivat helpottaa koodin kirjoittamista ja virheenkorjausta.

Voit myös lisätä muita paketteja ja kirjastoja TypeScript-projektiisi käyttämällä NPM-pakettienhallintaa ja `package.json` -tiedostoa.

## Katso myös

- TypeScriptin virallinen sivusto: https://www.typescriptlang.org/
- NPM-pakettienhallinta: https://www.npmjs.com/
- Visual Studio Code: https://code.visualstudio.com/