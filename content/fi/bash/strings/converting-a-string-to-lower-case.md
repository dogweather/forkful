---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
aliases:
- /fi/bash/converting-a-string-to-lower-case/
date:                  2024-01-20T17:37:54.363733-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Muuttaa kirjaimet pieniksi tekstissä. Auttaa vertailemaan sisältöä huomiottaen kirjainkoon.

## Miten:
```Bash
# Yksinkertainen tapa
teksti="Moi kaikki!"
pienet_kirjaimet=${teksti,,}
echo $pienet_kirjaimet
```
```
moi kaikki!
```

## Syväsukellus
Bashissa on alunperin käytetty tr-komentoa tai awk-ohjelmaa muuntamaan kirjaimet pieniksi. Vuoden 2000 jälkeen Bash versio 4.0 toi mukanaan sisäänrakennetut string-toiminnot, kuten yllä olevan lowercase-muunnoksen. Vaihtoehtoina voidaan mainita `tr`, `awk` ja modernit työkalut kuten `sed` ja Perl-skriptit.

```Bash
# tr-komennon käyttö
echo "Moi Kaikki!" | tr '[:upper:]' '[:lower:]'
```

Komento `tr` käsittelee merkkijonovirtoja, ja suorittaa annettujen joukkojen (`[:upper:]` ja `[:lower:]`) korvauksia tai poistoja.

Käyttöjärjestelmän ja ympäristöjen eroista johtuen, käytä Bashin sisäisiä toimintoja portabiliteetin vuoksi. Esimerkiksi Mac OS X:n ja vanhempien Linux-järjestelmien Bash-versiot eivät välttämättä tue yllä mainittuja sisäänrakennettuja string-toimintoja.

## Katso Myös
- Bash manual: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
- An Introduction to String Manipulation in Bash: https://linuxconfig.org/bash-scripting-tutorial-for-beginners
