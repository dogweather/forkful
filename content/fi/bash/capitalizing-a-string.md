---
title:    "Bash: Tekstin muuttaminen isoin kirjaimin"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Joskus Bash-ohjelmoijana sinun on ehkä tarvinnut muuttaa merkkijonon kirjainten kokoa. Tämä voi olla hyödyllistä esimerkiksi kun haluat korostaa tai erottaa tietyn osan merkkijonosta. Seuraavaksi kerron sinulle kuinka voit tehdä tämän Bash-ohjelmointikielen avulla.

## Kuinka tehdä

Voit muuttaa merkkijonon kirjainten kokoa yhdellä komennolla: `tr "[:lower:]" "[:upper:]" < file.txt` Tämä komento muuttaa kaikki file.txt -tiedostossa olevat pienimmät kirjaimet suuriksi kirjaimiksi. Voit myös käyttää tätä komentoa suoraan komentoriviltä.

```
Bash
#!/bin/bash

string="Tervetuloa suomeen"
echo "Alkuperäinen merkkijono: $string"

upper=$(echo "$string" | tr "[:lower:]" "[:upper:]")
echo "Suuri kirjain nyt: $upper"

lower=$(echo "$upper" | tr "[:upper:]" "[:lower:]")
echo "Pienet kirjaimet nyt: $lower"
```

Tässä esimerkissä luomme uuden merkkijonon `upper` ja `lower` -muuttujilla. Ensimmäisessä komennossa käytämme `tr` -toimintoa muuttaaksemme pienet kirjaimet suuriksi ja tallennamme sen uuteen muuttujaan `upper`. Toisessa komennossa muutamme suuret kirjaimet takaisin pieniksi käyttämällä `lower` -muuttujaa. Voit kokeilla tätä esimerkkiä omassa Bash-ympäristössäsi.

## Syväsukellus

Käyttämällä `tr` -komennon "[:lower:]" ja "[:upper:]" parametreja voit muokata muitakin merkkijonon ominaisuuksia. Esimerkiksi jos haluat muuttaa tekstissä esiintyvät numerot kirjaimiksi, voit käyttää "[:digit:]" ja "[:alpha:]" parametreja. Voit myös yhdistellä erilaisia ​​parametreja saadaksesi monimutkaisempia muokkauksia.

## Katso myös

- [Linux-komentorivin opas](https://help.ubuntu.com/community/UsingTheTerminal)
- [Bash-skriptaus opas](https://en.wikibooks.org/wiki/Bash_Shell_Scripting)