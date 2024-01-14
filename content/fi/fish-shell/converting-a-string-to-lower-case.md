---
title:    "Fish Shell: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointikielissä on valmiina funktio tai metodi tekstin muuttamiseksi pieniksi (lowercase) tai isoiksi (uppercase) kirjaimiksi. Kuitenkin Fish Shell-ohjelmoijina, meidän täytyy itse luoda nämä toiminnot käyttäen muutamia Shellin komennoita.

## Miten

 ```Fish Shellin``` toiminnot textin muuttamiseksi tarjoavat meille mahdollisuuden luoda `to-lower` funktio, joka muuttaa annetun merkkijonon pieniksi kirjaimiksi.

```
function to-lower
    set -l string $argv[1]
    echo $string | tr "[A-Z]" "[a-z]"
end
```

Tässä esimerkissä luodaan `to-lower` funktio, joka ottaa ensimmäisen argumentin ja tallentaa sen muuttujaan ```string```. Tämän jälkeen käytetään `tr` komentoa muuttamaan kaikki isot kirjaimet pieniksi kirjaimiksi.

### Esimerkki:

```
to-lower "HeLLo WoRLd"
```

Output: `hello world`

## Syvemmälle

Huomaa, että yllä olevassa esimerkissä käytimme ```tr``` komentoa, joka muuttaa kaikki isot kirjaimet pieniksi kirjaimiksi. Jos haluamme muuttaa ainoastaan ensimmäisen kirjaimen pieneksi, voimme käyttää `string` komentoa.

```
function to-lower-first
    set -l string $argv[1]
    set -l first_char (string split -m 1 " " $string[1])
    set -l rest_str (string join "" $string[2..-1])
    echo (string toupper $first_char)$rest_str
end
```

Tässä käytämme `string` komentoa jakamaan annetun merkkijonon ensimmäiseen kirjaimeen ja loput merkkijonosta. Sitten muutamme ensimmäisen kirjaimen isoksi ja lisäämme sen loppuun jäljellä olevan merkkijonon kanssa.

### Esimerkki:

```
to-lower-first "HeLLo WoRLd"
```

Output: `heLLo World`

## Katso myös

- [Fish Shell dokumentaatio](https://fishshell.com/docs/current/index.html)
- [tr command man page](https://linux.die.net/man/1/tr)
- [string command documentation](https://fishshell.com/docs/current/cmds/string.html)