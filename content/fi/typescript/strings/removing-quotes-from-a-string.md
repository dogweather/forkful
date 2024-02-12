---
title:                "Merkkijonosta lainausmerkkien poistaminen"
aliases:
- /fi/typescript/removing-quotes-from-a-string/
date:                  2024-01-26T03:42:25.143167-07:00
model:                 gpt-4-0125-preview
simple_title:         "Merkkijonosta lainausmerkkien poistaminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonosta lainausmerkkien poistaminen tarkoittaa ympäröivien yksittäisten (`'`) tai kaksinkertaisten (`"`) lainausmerkkien poistamista, jotka määrittävät merkkijono-literaaleja koodissa. Ohjelmoijat tekevät näin useista syistä, kuten tulosteen muotoilu, käyttäjäsyötteen puhdistaminen tai merkkijonojen valmistelu jäsennykseen tai tallennukseen, joissa lainausmerkit ovat tarpeettomia tai saattavat aiheuttaa virheitä.

## Kuinka:
Tässä on suoraviivainen opas niiden kiusallisten lainausmerkkien irrottamiseksi merkkijonoistasi TypeScriptillä.

```typescript
// Vaihtoehto A: Korvaa yksittäiset tai kaksinkertaiset lainausmerkit käyttäen regexiä
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"Lainausmerkeissä oleva merkkijono"`)); // Lainausmerkeissä oleva merkkijono
console.log(removeQuotes(`'Toinen tällainen'`)); // Toinen tällainen

// Vaihtoehto B: Käsittelee merkkijonoja, jotka alkavat ja päättyvät erilaisiin lainausmerkkeihin
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Epäsuhtaista'"`)); // "Epäsuhtaista'

// Vaihtoehto C: Poistaa useita erityyppisiä lainausmerkkejä
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Sekoitusta'n'Sovitusta'"`)); // Sekoitusta'n'Sovitusta
```

## Syväsukellus
Kauan ennen kuin TypeScript oli edes olemassa, JavaScript-koodarit jo kävivät läpi lainausmerkkitempuiluja, ja tarina on pitkälti sama TypeScriptille. Aikojen muuttuessa myös meidän tapamme viipaloida merkkijonoja muuttuu. Nykyään regexin lihaksikkuuden avulla hylkäämme kömpelöt merkkijonoviipaloinnit tai muut työläät menetelmät.

Vaikka yllä olevien esimerkkien pitäisi kattaa useimmat tarpeesi, muista, että lainaus voi olla monimutkaista. Sisäkkäiset, epäsuhtaista ja paennut lainausmerkit ovat konnuja, jotka odottavat kompastuttamaan sinua. Näitä varten saatat tarvita monimutkaisempia malleja tai jopa jäsennyslaitteita käsittelemään jokaista kiemuraista tapausta.

Vaihtoehtoja? Jotkut tykkäävät käyttää kirjastoja, kuten lodash, jossa on metodeja kuten `trim` ja `trimStart` / `trimEnd`, jotka voidaan räätälöidä leikkaamaan lainausmerkkejä, jos asetat hahmot, jotka haluat nipistää.

Ja sinä TypeScript-entusiastit, älkäämme unohtako tyyppejä. Vaikka tässä käsittelemme pääasiassa merkkijonoja, kun työskentelet käyttäjäsyötteiden kanssa tai jäsentelet, tyyppivartijoiden tai jopa geneerikkojen lisääminen voi auttaa varmistamaan, että pidät koodisi yhtä turvallisena kuin lainausmerkkisi ovat leikattuja.

## Katso myös
Tutustu näihin virtuaalisiin keskuksiin saadaksesi lisätietoja:

- MDN Web Docs regexistä (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- TypeScriptin virallinen dokumentaatio (https://www.typescriptlang.org/docs/)
- You Don't Need Lodash/Underscore – Merkkijonoapuohjelmat (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: Traversoi juoksuhaudat, joissa lukemattomat kehittäjät ovat taistelleet lainausmerkkikatastrofeja vastaan (https://stackoverflow.com/)
