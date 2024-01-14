---
title:                "TypeScript: Tietorakenteiden yhdistäminen"
simple_title:         "Tietorakenteiden yhdistäminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi
Miksi haluat käyttää TypeScriptiä merkkijonojen konkatenointia?
Merkkijonojen konkatenointi on tärkeä osa ohjelmoinnin, joka mahdollistaa yhdistämisen kaksi tai useampia merkkijonoja yhdeksi. Tämä tekee koodisi helpommin luettavaksi ja ymmärrettäväksi.

## Kuinka tehdä
Käyttämällä TypeScriptiä, merkkijonot voidaan konkatenoida käyttämällä "+" merkkiä. Katso alla olevaa esimerkkiä:

```TypeScript
let etunimi: string = "Juha";
let sukunimi: string = "Virtanen";
let kokoNimi: string = etunimi + " " + sukunimi;

console.log(kokoNimi); // tulostaa "Juha Virtanen"
```

## Syvällinen sukellus
 TypeScriptin konkatenointi toimii hieman eri tavalla kuin muiden ohjelmointikielten. Sen sijaan, että suorittaisit konkatenoinnin suoraan merkkijonojen välillä, TypeScript luo uuden merkkijonon jokaisen "+" merkin jälkeen. Tämä voi aiheuttaa suorituskyvyn hidastumisen, jos käytetään suurta määrää merkkijonoja.

## Katso myös
- [TypeScriptin virallinen dokumentaatio](https://www.typescriptlang.org/docs/)
- [Miksi TypeScript on hyvä valinta?](https://www.freecodecamp.org/news/is-typescript-worth-learning-getting-started/)
- [Miten selviät TypeScript-koodin hankaluuksista?](https://levelup.gitconnected.com/how-to-tackle-the-tricky-parts-of-typescript-8275ed8762f)