---
title:                "TypeScript: Muunna päivämäärä merkkijonoksi"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Päivämäärien muuttaminen merkkijonoksi voi olla hyödyllistä monessa tilanteessa, kuten tallentaessa tai esittäessä päivämääriä tietokannoissa tai käsiteltäessä käyttäjän syöttämiä päivämääriä. TypeScript tarjoaa helpon tavan muuttaa päivämäärä merkkijonoksi käyttämällä Date- ja toLocaleString-metodeja.

## Kuinka

```typescript
const paivamaara = new Date();
console.log(paivamaara.toLocaleString("fi-FI")); 
```

Tämä koodinpätkä luo uuden Date-objektin ja käyttää sitten toLocaleString-metodia muuttaakseen sen merkkijonoksi. Metodille annetaan parametrina haluttu kielikoodi, tässä tapauksessa "fi-FI", joka tuottaa päivämäärän suomalaisessa muodossa.

Tämän lisäksi voimme myös muokata päivämäärän esitystä haluamallamme tavalla antamalla toLocaleString-metodille parametreina välimerkkejä, kuten alla olevassa esimerkissä:

```typescript
const paivamaara = new Date();
console.log(paivamaara.toLocaleString("fi-FI", {weekday: "long", year: "numeric", month: "long", day: "numeric"})); 
```

Tällöin tuloksena on esimerkiksi "torstai 15. heinäkuuta 2021".

## Syvempi sukellus

Päivämäärien muuttaminen merkkijonoksi perustuu Date-olion toLocaleString-metodin käyttämiseen. Tämä metodi hakee tietokoneen asetuksista kyseiselle kielikoodille määritellyn päivämäärämuodon. Tämän avulla voimme muokata tulostettavan päivämäärän esitystä eri tavoin käyttämällä annettuja parametreja.

On myös hyvä huomata, että päivämäärät tallennetaan tietokoneilla yleensä millisekunteina, joten ne tulee ensin muuttaa Date-objektiksi, jotta voimme käyttää toLocaleString-metodia.

## Katso myös

- [Date-objektin dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [toLocaleString-metodin dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)