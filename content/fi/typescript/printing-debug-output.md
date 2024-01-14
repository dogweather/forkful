---
title:    "TypeScript: Tulostaminen virheenjäljitystiedostoon"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Debuggaustulosteiden tulostaminen on tärkeä osa ohjelmoinnin prosessia. Se auttaa kehittäjiä tunnistamaan ja korjaamaan virheitä sekä parantamaan koodin suorituskykyä. Lisäksi tulosteiden avulla voi seurata ohjelman suorituksen kulkua ja löytää mahdollisia optimointimahdollisuuksia.

## Kuinka

```TypeScript
// Luodaan yksinkertainen funktio, joka tulostaa kaksi muuttujaa yhteen laskettuna
function tulostaSumma(a: number, b: number): void {
  console.log("Summa: " + (a + b));
}

// Kutsutaan funktiota ja annetaan parametreiksi kaksi lukua
tulostaSumma(5, 10);

// Tulostus:
// Summa: 15
```

Koodiesimerkissä luodaan yksinkertainen funktio, joka ottaa vastaan kaksi lukua ja tulostaa niiden summan. Funktioon kutsuttaessa tulokseksi saadaan "Summa: 15". Scriptin suorituksen aikana tuloste näkyy konsolissa tai selaimen kehitystyökaluissa, kuten Chrome DevToolsissa.

## Syvempi sukellus

Tulosteiden lisääminen koodiin on erittäin tärkeää virheiden ja suorituskyvyn parantamisen kannalta, mutta liiallinen tulostaminen voi hidastaa ohjelman suoritusta. On tärkeää löytää tasapaino tulosteiden määrän suhteen ja poistaa turhat tulosteet lopullisesta tuotantoversiosta.

Toinen tärkeä huomioitava asia on tulosteiden muotoilu ja selkeys. Hyvä tapa on lisätä tulosteisiin tarkentavaa tekstiä, kuten funktion nimi tai laskutoimituksen selitys. Tämä auttaa hahmottamaan koodin toimintaa ja helpottaa mahdollisten virheiden löytämistä.

## Katso myös

- [TypeScript Debugging Documentation](https://www.typescriptlang.org/docs/handbook/debugging.html)
- [The Importance of Debugging in Programming](https://generalassemb.ly/blog/importance-debugging-computer-science/)
- [Using Console.log() Effectively](https://app.pluralsight.com/guides/using-console.log-effectively)