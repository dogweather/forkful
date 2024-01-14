---
title:                "TypeScript: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Haluatko tietää kuinka monta merkkiä on kussakin sanassa? Tai ehkä haluat tarkistaa, onko annettu merkkijono tarpeeksi pitkä ennen kuin lähetät sen palvelimelle. Oli syysi mikä tahansa, on tärkeää tietää kuinka löytää merkkijonon pituus TypeScriptissä.

## Kuinka

```TypeScript
let sana: string = "Tervetuloa";
console.log(sana.length); // Output: 10
```

Käyttämällä `.length` metodia, voit helposti löytää annetun merkkijonon pituuden TypeScriptissä. Tässä esimerkissä olemme alustaneet muuttujan `sana` arvolla "Tervetuloa" ja sitten tulostaneet sen pituuden konsoliin.

Voit myös käyttää tätä metodia suoraan merkkijonoon ilman muuttujaa:

```TypeScript
console.log("Hei maailma".length); // Output: 11
```

## Syvällinen perehtyminen

`.length` metodi palauttaa numeron, joka vastaa merkkijonon merkkien määrää. Tämä sisältää myös välilyönnit ja muut erikoismerkit. Se on erittäin hyödyllinen esimerkiksi silloin kun haluat varmistaa, että käyttäjän antama syöte on tarpeeksi pitkä ennen kuin käsittelet sitä. Voit myös käyttää `.length` metodia for-silmukassa käsitelläksesi jokaisen merkin erikseen.

On myös hyvä huomata, että `.length` metodi toimii myös muiden tietotyyppien, kuten numeroiden ja boolean-arvojen, kanssa. Se palauttaa niiden "pituuden" eli numeron merkkien määrästä, jotka ovat tarvittavia sen esittämiseen.

## Katso myös

- [String.prototype.length - MDN web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [TypeScript Strings - W3Schools](https://www.w3schools.com/js/js_strings.asp)
- [Lukujen ja merkkijonojen vertailu | TypeScript-tutoriaali](https://typescript-tutoriaalit.net/numerot/vertailu/)