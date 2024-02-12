---
title:                "Päivämäärän jäsennys merkkijonosta"
date:                  2024-02-03T19:15:54.585869-07:00
model:                 gpt-4-0125-preview
simple_title:         "Päivämäärän jäsennys merkkijonosta"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkkijonosta päivämäärän jäsentäminen tarkoittaa päivämäärien ja aikojen tekstiesitysten muuntamista muotoon, jota ohjelma voi käsitellä ja analysoida. Tämä on yleinen tehtävä ohjelmoinnissa, koska se mahdollistaa käyttäjän syötteen käsittelyn, aikaleimattujen tietojen tallennuksen ja API-rajapintojen kanssa toimimisen, mikä tuottaa toiminnallisempia ja käyttäjäystävällisempiä sovelluksia.

## Kuinka:
TypeScript, ollessaan JavaScriptin yliluokka, nojaa Date-objektiin merkkijonoista päivämäärien jäsentämiseksi. Kuitenkin päivämäärien käsittely JS/TS:ssä voi muuttua monisanaiseksi tai epätarkaksi johtuen Date-objektin omituisuuksista. Tässä on perusesimerkki, minkä jälkeen tulee lähestymistapa käyttäen suosittua kirjastoa, `date-fns`, kestävämpiin ratkaisuihin.

### JavaScriptin Date-objektin käyttö
```typescript
// Perusjäsennys käyttäen Date-konstruktoria
const dateFromString = new Date("2023-04-21T15:00:00Z");
console.log(dateFromString.toString()); 
// Tuloste GMT:lle: "Fri Apr 21 2023 15:00:00 GMT+0000 (Koordinoitu yleisaika)"
```

Tämä menetelmä toimii ISO-muotoisten merkkijonojen ja joittenkin muiden päivämäärämuotojen kanssa, mutta voi tuottaa epäjohdonmukaisia tuloksia epäselvien muotojen kanssa eri selaimissa ja paikallisasetuksissa.

### date-fns:n käyttö
`date-fns`-kirjasto tarjoaa suoraviivaisen ja johdonmukaisen tavan käsitellä päivämääriä. Se on modulaarinen kirjasto, joka mahdollistaa vain tarvitsemiisi osiin keskittymisen, vähentäen paketin kokoa.

Asenna ensin `date-fns`: 

```sh
npm install date-fns
```

Käytä sen jälkeen sitä merkkijonosta päivämäärän jäsentämiseen:

```typescript
import { parseISO, format } from 'date-fns';

// ISO-merkkijonon jäsentäminen
const dateString = "2023-04-21T15:00:00Z";
const parsedDate = parseISO(dateString);

// Päivämäärän muotoilu (esim. ihmislukukelpoiseen muotoon)
console.log(format(parsedDate, "PPPpp")); 
// Tuloste: "Apr 21st, 2023 at 3:00 PM" (tuloste voi vaihdella paikallisasetusten mukaan)
```

`date-fns` tukee laajaa valikoimaa muotoja ja paikallisia asetuksia, tehden siitä vankan valinnan sovelluksille, jotka vaativat tarkkaa päivämäärän jäsentämistä ja muotoilua eri käyttäjäalueilla.
