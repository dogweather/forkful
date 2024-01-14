---
title:                "TypeScript: Merkkijonon suurennus"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat muuttaa merkkijonon ensimmäisen kirjaimen isoiksi kirjaimiksi TypeScriptissä. Se voi auttaa tekstin luettavuudessa, noudattaa ohjelmointityyliäsi tai yksinkertaisesti näyttää ammattimaisemmalta.

## Miten

Voit muuttaa merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi TypeScriptissä käyttämällä sisäänrakennettua.toUpperCase() -metodia. Seuraava koodiesimerkki näyttää, kuinka tämä tehdään:

```typescript
let merkkijono = "tere" 
console.log (["Uusi merkkijono: " + merkkijono[0].toUpperCase () + merkkijono.slice (1)]);
```

Tulostus olisi:

```typescript
Uusi merkkijono: Tere
```

Voit myös käyttää tätä metodia muille kirjaimille merkkijonon sisällä. Tässä esimerkki, joka muuttaa jokaisen sanan ensimmäisen kirjaimen isoksi kirjaimeksi:

```typescript
let tekstinMerkkijono = "terve maailma" 
console.log (tekstinMerkkijono.split (''). map (sana => sana.charAt (0) .toUpperCase () + sana.slice (1). Join ('')));
```

Tulostus olisi:

```typescript
Terve Maailma
```

## Syvällinen sukellus

Toinen tapa muuttaa merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi on käyttää Regular Expressionsia. Voit luoda säännöllisen ilmauksen, joka tunnistaa ensimmäisen merkin ja korvaa sen isolla kirjaimella. Seuraavassa koodiesimerkissä käytetään replace-metodia ja säännöllistä lausetta vaihtamaan "h" isolla "H":lla:

```typescript
let säännöllinenLauseke = /(^h)/g 
let merkkijono = "hei" 
console.log (merkkijono.replace (säännöllinenLauseke, "H"));
```

Tulostus olisi:

```typescript
Hei
```

On tärkeää muistaa, että tällä tavalla et voi muuttaa kaikkia merkkejä isoksi kirjaimeksi, vain ensimmäisen merkin.

## Katso myös

- [toUpperCase ()](https://www.w3schools.com/jsref/jsref_touppercase.asp)
- [replace ()](https://www.w3schools.com/jsref/jsref_replace.asp)
- [Regular Expressions](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)