---
title:                "Bash: Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi?

Miksi haluat muuntaa merkkijonon pieniksi kirjaimiksi? Tämä voi olla hyödyllistä esimerkiksi silloin, kun haluat vertailla merkkijonoja tai etsiä tiettyä sanaa merkkijonosta.

## Miten?

### Bash-komentorivi

Voit käyttää Bash-komentorivillä `tr`-komennolla pienentämään kaikki merkkijonossa olevat kirjaimet.

```Bash
merkkijono="TÄMÄ ON ESIMERKKI"
echo "$merkkijono" | tr '[:upper:]' '[:lower:]'
```

Tämä tulostaa:

```Bash
tämä on esimerkki
```

Voit myös käyttää `awk`-komennolla pienentämään vain tiettyä osaa merkkijonosta, esimerkiksi toisen sanan.

```Bash
merkkijono="TÄMÄ ON ESIMERKKI"
echo "$merkkijono" | awk '{ print tolower($2) }'
```

Tämä tulostaa:

```Bash
on
```

### Bash-skripti

Voit myös luoda Bash-skriptin, joka muuntaa merkkijonon pieniksi kirjaimiksi käyttäen `tr`-komennon sijaan `sed`-komentoa.

```Bash
#!/bin/bash

echo "Anna merkkijono:"
read merkkijono

pieni_merkkijono=$(echo $merkkijono | sed 's/.*/\L&/')

echo "$pieni_merkkijono"
```

Skripti kysyy käyttäjältä merkkijonoa ja tulostaa sen pienissä kirjaimissa.

## Syvällisempi sukellus

Merkkijonon muuntaminen pieniksi kirjaimiksi ei ole monimutkaista, mutta voit syventää ymmärrystäsi siitä, miten muuntaminen tapahtuu.

`tr`-komennolla muuntaminen perustuu merkkilistaan, johon määritetään merkit, jotka muunnetaan pieniksi kirjaimiksi ja merkit, joita ei muunneta. Voit tutustua tarkemmin merkkilistojen rakentamiseen `man tr`-komennolla.

`sed`-komennolla tapahtuva muuntaminen perustuu säännöllisiin lausekkeisiin. Esimerkiksi `[A-Z]` tarkoittaa kaikkia isoja kirjaimia ja `(?<!\\)\L` muuntaa edellisen säännöllisen lausekkeen mukaiset merkit pieniksi kirjaimiksi. Voit lukea lisää säännöllisistä lausekkeista `man sed`-komennolla.

## Katso myös

- [Bash-komentorivin perusteet](https://www.datacamp.com/community/tutorials/bash-shell-tutorial)
- [Merkkilistojen rakentaminen `tr`-komennossa](https://www.computerhope.com/unix/utr.htm)
- [Säännölliset lausekkeet `sed`-komennossa](https://linuxhint.com/regex_sed_command/)