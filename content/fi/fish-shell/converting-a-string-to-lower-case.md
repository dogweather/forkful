---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Fish Shell: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Miksi 

On monia syitä, miksi haluat muuttaa merkkijonon pieniksi kirjaimiksi. Yksi yleinen syy on, että haluat varmistaa, että merkkijono on yhtenäisessä muodossa ja omalla kielelläsi. Tämä on hyödyllistä esimerkiksi vertaillessa merkkijonoja keskenään.

# Miten tehdä

Fish Shellilla merkkijonon muuttaminen pieniksi kirjaimiksi on helppoa. Käytä vain `string tolower` komentoa, ja anna haluamasi merkkijono parametrina.

```
Fish Shell
string tolower "TÄMÄ ON MERKKIJONO"
```

Tämä koodi tuottaa seuraavan tulosteen:

```
tämä on merkkijono
```

# Syvemmälle

Fish Shellin `string tolower` komento toimii muuntamalla merkkijonon jokaisen kirjaimen pieneksi kirjaimeksi. Tämä tarkoittaa, että se myös muuttaa kirjaimet, jotka ovat jo pieniä kirjaimia, kuten aakkoset ja välimerkit.

Voit myös käyttää `string match -r` komentoa yhdistettynä `string tolower` komentoon, jotta muuntaisit vain tiettyjä kirjaimia haluamassasi merkkijonossa. Esimerkiksi, jos haluat muuttaa vain sanat "fish" ja "shell" merkkijonossa pieniksi kirjaimiksi, voit käyttää seuraavaa koodia:

```
fish shell
string match -r "(fish|shell)" | string tolower
```

Tämä tuottaa tuloksen:

```
fish shell
```

# Katso myös

- Fish Shellin virallinen dokumentaatio: https://fishshell.com/docs/current/index.html
- "String Processing in Fish Shell" opetusohjelma: https://www.tecmint.com/useful-string-processing-features-in-fish-shell/
- "Learn Fish Shell: A Beginner's Guide" opetusohjelma: https://medium.com/swlh/learn-fish-shell-a-beginner-s-guide-6b62d64c4383