---
date: 2024-01-26 03:40:59.053178-07:00
description: "Miten: Kuvittele, ett\xE4 sinulla on merkkijono, joka on kiedottu lainausmerkkeihin,\
  \ kuten `\"\\\"Hello, World!\\\"\"` ja haluat puhtaan, lainaamattoman tekstin.\u2026"
lastmod: '2024-03-13T22:44:56.938104-06:00'
model: gpt-4-0125-preview
summary: "Kuvittele, ett\xE4 sinulla on merkkijono, joka on kiedottu lainausmerkkeihin,\
  \ kuten `\"\\\"Hello, World!\\\"\"` ja haluat puhtaan, lainaamattoman tekstin."
title: Merkkijonosta lainausmerkkien poistaminen
weight: 9
---

## Miten:
Kuvittele, että sinulla on merkkijono, joka on kiedottu lainausmerkkeihin, kuten `"\"Hello, World!\""` ja haluat puhtaan, lainaamattoman tekstin. Tässä on nopea JavaScript-katkelma vapauttamaan merkkijonosi näistä lainausmerkki kahleista:

```javascript
let quotedString = "\"Hello, World!\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // Tulostus: Hello, World!
```

Entä jos käsittelet yksittäisiä lainausmerkkejä? Muokkaa vain regexiä hieman:

```javascript
let singleQuotedString = "'Hello, World!'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // Tulostus: Hello, World!
```

Entä jos merkkijonosi on sekoitus molempia? Ei hikiä:

```javascript
let mixedQuotedString = "\"'Hello, World!'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // Tulostus: 'Hello, World!'
```

## Syväsukellus
Ennen JSONin valta-aikaa lainausmerkkien välttäminen oli villi länsi kauttaviivoista ja kikoista. Varhaiset ohjelmointikielet eivät aina tulleet toimeen lainausmerkkien kanssa, mikä tarkoitti paljon manuaalista merkkijonomanipulaatiota. Nyt, standardoitujen dataformaattien kanssa, lainausmerkkien poistaminen on usein kyse syötteiden siivoamisesta ennen niiden käsittelyä JSONina tai tekstin tallentamisesta ilman muotoiluristiriitoja.

Vaihtoehdot `.replace()`-metodille? Toki! Voisit halkaista ja yhdistää merkkijonon lainausmerkkien kohdalta, käyttää `slice`-metodia, jos olet varma lainausmerkkiesi sijainnista, tai jopa regex-hakua vetääksesi esiin tarvittavan tekstin. Kaikki riippuu kontekstista.

Mutta älä unohda reunatapauksia: lainausmerkit lainausmerkkien sisällä, escapetut lainausmerkit ja kansainväliset merkit. Ajattele merkkijonoasi mahdollisena poikkeusten miinakenttänä ja kulje varoen. Nykyaikaiset JavaScript-moottorit on optimoitu käsittelemään regex-operaatioita tehokkaasti, joten ne ovat yleensä ensisijainen valinta, mutta raskaiden datankäsittelytehtävien suorituskykyä on aina syytä tarkkailla.

## Katso myös
Sukella syvemmälle merkkijonomanipulaatioon ja regexiin:

- Mozilla Developer Network String.replace()-metodista: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 testataksesi regex-mallejasi: https://regex101.com/
- JSON.org ymmärtääksesi, miksi käsittelemme niin monia lainausmerkkejä modernissa web-kehityksessä: http://json.org/
