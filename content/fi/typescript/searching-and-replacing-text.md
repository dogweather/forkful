---
title:                "TypeScript: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi
Miksi etsiä ja korvata tekstiä TypeScript-ohjelmoinnissa? Yksinkertaisesti sanottuna, se tekee merkkijonojen manipuloinnista ja muokkaamisesta helpompaa ja tehokkaampaa. Tämä voi olla erityisen hyödyllistä esimerkiksi tekstin käsittelyssä tai datan käsittelyssä.

## Kuinka tehdä
Etsi ja korvaa tekstiä TypeScriptissä on melko yksinkertaista käyttämällä sisäänrakennettuja metodikutsuja, kuten "replace". Tässä esimerkissä korvaamme kaikki "koira" merkkijonot "kissa" merkkijonoilla:

```TypeScript
let alkuperäinenTeksti = "Minulla on koira nimeltä Max.";
let uusiTeksti = alkuperäinenTeksti.replace("koira", "kissa");
console.log(uusiTeksti);
```
Tulos:

```
Minulla on kissa nimeltä Max.
```

Voimme myös käyttää säännöllisiä lausekkeita etsimään ja korvaamaan tiettyjä merkkijonoja. Esimerkiksi korvaamme kaikki numerot merkkijonossa tyhjillä:

```TypeScript
let alkuperäinenTeksti = "Tänä vuonna olen jo syönyt 12 jäätelöä.";
let uusiTeksti = alkuperäinenTeksti.replace(/\d/g, "");
console.log(uusiTeksti);
```
Tulos:

```
Tänä vuonna olen jo syönyt jäätelöä.
```

## Tarkempi tarkastelu
On tärkeää huomata, että "replace" -metodi korvaa vain ensimmäisen löydön merkkijonossa. Jos haluamme korvata kaikki esiintymät, meidän on käytettävä säännöllisiä lausekkeita ja "g" -lippua. Voimme myös käyttää "i" -lippua poistamaan kirjainkoon eroja.

Lisäksi "replace" -metodi ei muokkaa alkuperäistä merkkijonoa, vaan palauttaa muokatun version. Jos haluamme muokata alkuperäistä tarkalleen, voimme käyttää "replaceAll" -metodia.

Nämä ovat vain muutamia esimerkkejä, mutta etsi ja korvaa tekstin syvällisempi tarkastelu voisi tulla esiin, kun tarkennetaan tiettyjä käyttötapoja ja ongelmia, joita voi esiintyä.

## Katso myös
- <a href="https://www.typescriptlang.org/docs/handbook/basic-types.html" target="_blank">TypeScriptin perustyypit</a>
- <a href="https://regexr.com/" target="_blank">Säännöllisten lausekkeiden testaustyökalu</a>
- <a href="https://www.w3schools.com/jsref/jsref_replace.asp" target="_blank">JS:n replace-metodin dokumentaatio</a>