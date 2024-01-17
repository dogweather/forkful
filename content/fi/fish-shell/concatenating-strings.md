---
title:                "Jonojen yhdistäminen"
html_title:           "Fish Shell: Jonojen yhdistäminen"
simple_title:         "Jonojen yhdistäminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

"Konkatenaatio" on ohjelmoinnin termi, joka tarkoittaa merkkijonojen yhdistämistä toisiinsa. Tämä voi olla hyödyllistä esimerkiksi silloin, kun halutaan luoda uusi merkkijono, joka sisältää useita erillisiä osia. Ohjelmoijat käyttävät konkatenaatiota, jotta voivat luoda dynaamisesti muuttuvia merkkijonoja, jotka vastaavat tiettyjä tarpeita, kuten tulostusta tai tiedostojen luontia.

# Miten:


Fish Shell tarjoaa kätevän ja helppokäyttöisen tavan yhdistää merkkijonoja. Voit käyttää "string join" -komennolla yhdistää merkkijonoja erilaisilla erottimilla tai jättää eroittimen kokonaan pois. Alla on esimerkki koodista ja tulosteesta:

```
Fish Shell string join example:
$ set fruits peach mango banana
$ echo (string join , $fruits)
peach, mango, banana
```

# Syvemmälle:

Konkatenaatio on ollut käytössä jo pitkään ohjelmoinnissa, ja se on edelleen suosittu ratkaisu tähän päivään asti. Sen lisäksi, että Fish Shell tarjoaa helpon tavan yhdistää merkkijonoja, on olemassa myös muita vaihtoehtoja kuten "string concatenation" -funktiot muissa kielissä kuten Python ja Java. Konkatenaation toteuttaminen Fish Shellissä perustuu useisiin algoritmeihin, jotka optimoivat muistinvarauksia ja parantavat suorituskykyä.

# Katso myös:

Voit lukea lisää Fish Shellin string join -komennosta dokumentaatiosta osoitteessa https://fishshell.com/docs/current/cmds/string.html#join ja tutustua muihin merkkijonojen käsittelyyn liittyviin komentoihin kuten string split ja string length.