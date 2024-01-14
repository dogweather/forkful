---
title:    "Fish Shell: Kirjoittaminen standardivirheelle"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi Kirjoittaa Standard Erroriin?

Kirjoittaminen standard erroriin on tärkeä taito Fish Shell -ohjelmointikielessä, jota jokaisen ohjelmoijan tulisi hallita. Se mahdollistaa virheiden käsittelyn ja viestien näyttämisen käyttäjälle. Se on myös hyödyllinen ohjelman debuggaamisessa ja suorituksen seuraamisessa.

## Kuinka Teet Sen?

Fish Shellissa voit kirjoittaa standard erroriin käyttämällä "echo" komentoa ja uudelleenohjaamalla sen symbolilla ">&2". Tämä näyttää viestin käyttäjälle standard error kanavan kautta.

```
Fish Shell koodin esimerkki

echo "Tämä on viesti virheelliseen tulosteeseen" >&2
```

Tämä komento tulostaa viestin "Tämä on viesti virheelliseen tulosteeseen" käyttäjän näytölle.

## Syvempi Sukellus

On tärkeää huomata, että standard error on erillinen kanava standard outputista (käyttäjän näyttö). Tämä tarkoittaa sitä, että voit ohjata virheviestit eri sijaintiin kuin normaali tuloste. Voit myös ohjata molemmat tulosteet samaan sijaintiin käyttämällä symbolia "2>&1". On myös muita hyödyllisiä Fish Shellin komentoja, kuten "set -e", joka lopettaa ohjelman suorituksen välittömästi, jos se kohtaa virheen.

## Katso myös
- [Fish Shellin dokumentaatio standard errorista](https://fishshell.com/docs/current/index.html#output-redirection)
- [Hyödyllisiä komentoja Fish Shellissa](https://fishshell.com/docs/current/commands.html)