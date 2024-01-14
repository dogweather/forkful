---
title:                "Javascript: Vianmääritystulostus tietokoneohjelmoinnissa"
simple_title:         "Vianmääritystulostus tietokoneohjelmoinnissa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Usein kehittäjät joutuvat korjaamaan ohjelmointivirheitä, joita voi olla haastavaa paikantaa koodista. Käyttämällä debuggaustoimintoja ja tulostamalla debuggaustietoja koodiin, voit helpommin havaita ja korjata nämä virheet. Tai ehkä haluat yksinkertaisesti tarkistaa tietokannan tulokset tai muuttujien arvot koodin suorituksen aikana. Ei väliä mikä on syysi, tulostamisella debug-tietoja voi olla paljon hyötyä ohjelmointiprosessissa.

## Miten

Tulostaminen debug-tietoja JavaScriptissä on helppoa. Voit käyttää `console.log()` -funktiota tulostamaan viestejä komentotulkkauksessa tai selaimen konsolissa. Voit myös käyttää `console.error()` tulostamaan virheviestejä tai `console.warn()` varoitusviestejä.

```Javascript
console.log("Tämä on debug-viesti"); // Tulostaa "Tämä on debug-viesti" komentotulkkaukseen tai selaimen konsoliin.
let numero = 10;
console.log("Muuttujan arvo on: " + numero); // Tulostaa "Muuttujan arvo on: 10".
console.error("Virhe: yhtäsuuruusmerkki puuttuu laskutoimituksesta"); // Tulostaa virheviestin komentotulkkaukseen tai selaimen konsoliin.
console.warn("Varoitus: muuttuja ei ole alustettu"); // Tulostaa varoitusviestin komentotulkkaukseen tai selaimen konsoliin.
```

Voit myös tulostaa debug-tietoja suoraan HTML-sivulle käyttämällä `document.write()` -funktiota. Tämä on hyödyllistä, jos haluat tarkistaa muuttujien arvoja sivun suorituksen aikana.

```Javascript
document.write("Muuttujan arvo on: " + numero); // Tulostaa "Muuttujan arvo on: 10" suoraan HTML-sivulle.
```

## Syvempi sukellus

Tulostamalla debug-tietoja voit nähdä tarkalleen, mitä tapahtuu koodissasi suorituksen aikana. Voit käyttää ehtolausekkeita ja `console.assert()` -funktiota tulostamaan viestejä vain tietyissä tilanteissa. Esimerkiksi:

```Javascript
let luku = 5;

console.assert(luku > 10, "Virhe: lukuarvo on liian pieni"); // Tulostaa virheviestin vain, jos luku on alle 10.
```

Voit myös käyttää `console.group()` ja `console.groupEnd()` -funktioita ryhmitelläksesi ja selkeyttääksesi debug-viestejäsi.

```Javascript
console.group("Laskutoimitukset");
console.log("2 + 2 = " + (2+2));
console.log("5 * 3 = " + (5*3));
console.groupEnd(); // Ryhmä päättyy tähän.
```

## Katso myös

- [MDN - Console API](https://developer.mozilla.org/en-US/docs/Web/API/console)
- [Kehittäjäopas: debuggaaminen JavaScriptissä](https://www.w3schools.com/js/js_debugging.asp)
- [Debuggaus vinkkejä ja temppuja JavaScriptille](https://www.sololearn.com/learn/JavaScript/224/)