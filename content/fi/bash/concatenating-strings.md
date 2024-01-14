---
title:                "Bash: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi
On monia tilanteita, joissa meidän täytyy yhdistää kaksi tai useampaa tekstiriviä yhdeksi. Tämä voi olla hyödyllistä esimerkiksi silloin, kun luodaan dynaamisia tekstejä, kuten raportteja tai sähköpostiviestejä. Bash-ohjelmoinnissa on mahdollista yhdistää merkkijonoja, mikä tekee siitä tehokkaan työkalun tällaisten tehtävien ratkaisemiseen.

## Kuinka
Merkkijonojen yhdistäminen Bash-ohjelmoinnissa on helppoa ja nopeaa. Käytämme tätä varten "+" merkkiä tai "." merkkiä, riippuen siitä, mitä tarkalleen haluamme tehdä. Katso seuraava esimerkki:

```Bash
# Aseta muuttuja
firstname="Matti"
lastname="Meikäläinen"
# Yhdistä merkkijonot
fullname=$firstname" "$lastname
echo $fullname
```

Tässä esimerkissä meillä on kaksi muuttujaa, "firstname" ja "lastname", jotka sisältävät etunimen ja sukunimen. Käytämme sitten "+" merkkiä yhdistämään nämä kaksi muuttujaa yhdeksi "fullname" muuttujaksi. Lopuksi tulostamme tämän uuden muuttujan, joka näyttää "Matti Meikäläinen".

Voimme myös käyttää ". "merkkiä yhdistämään merkkijonoja. Tämä merkki lisää välilyönnin, joten voimme kirjoittaa esimerkiksi "firstname.lastame" sijaan "firstname lastname". Katso seuraava esimerkki:

```Bash
# Aseta muuttuja
firstname="Matti"
lastname="Meikäläinen"
# Yhdistä merkkijonot
email=$firstname"."$lastname"@example.com"
echo $email
```

Tässä olemme luoneet uuden muuttujan "email", joka sisältää kokonaisen sähköpostiosoitteen "Matti.Meikäläinen@example.com".

## Syvään sukellus
Bash-ohjelmoinnissa merkkijonojen yhdistäminen voi tehdä monimutkaisempien tehtävien ratkaisemisesta helpompaa. Voimme esimerkiksi käyttää "for" -silmukkaa yhdistämään useita merkkijonoja samalla kertaa. Voimme myös yhdistää merkkijonoja muiden muuttujien tai pikanäppäimien kanssa, kuten esimerkiksi $PWD, joka näyttää tämänhetkisen työhakemiston.

Lisäksi Bash-ohjelmoinnin lisäksi myös muut ohjelmointikielet, kuten Python, tukevat merkkijonojen yhdistämistä. Merkkijonojen yhdistämisestä syntyy usein myös keskustelua, koska jotkut pitävät sitä tehottomana tapana, kun taas toiset näkevät sen hyödyllisenä työkaluna tiettyihin tehtäviin.

## Katso myös
- https://www.tutorialspoint.com/unix/unix-string-concatenation.htm
- https://www.tecmint.com/concatenate-strings-in-bash-scripting/
- https://www.geeksforgeeks.org/concatenate-strings-bash-scripting/