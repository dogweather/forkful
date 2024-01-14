---
title:    "Bash: Tarkistetaan löytyykö hakemisto"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi: Miksi haluaisit tarkistaa, onko hakemisto olemassa?

Hakemiston olemassaolon tarkistaminen on tärkeä osa Bash-ohjelmointia, sillä se auttaa sinua varmistamaan, että olet oikeassa paikassa ja käsittelet oikeaa hakemistoa. Tämä voi säästää paljon aikaa ja vaivaa virheiden selvittämisessä ja tekee ohjelmoinnista yleisesti ottaen tehokkaampaa.

## Kuinka:

Voit tarkistaa hakemiston olemassaolon Bashissa käyttämällä "test" -komentoa ja "-d" -tunnusta, joka tarkoittaa directory (hakemisto englanniksi). "test" -komentoa käytetään yleensä ehdollisten lauseiden yhteydessä ja se palauttaa joko "true" tai "false" riippuen siitä, onko ehto täytetty.

```
Bash
test -d <hakemisto>
```

Jos haluat esimerkiksi tarkistaa, onko hakemisto "Kuvat" olemassa, koodi olisi:

```
Bash
test -d Kuvat
```

Jos hakemisto on olemassa, ohjelma palauttaa "true". Voit myös käyttää "if" -lauseketta yhdessä "test" -komenton kanssa käsittelemään erilaisia skenaarioita.

```
Bash
if test -d Kuvat
then
    echo "Hakemisto 'Kuvat' on olemassa."
else
    echo "Hakemistoa ei löytynyt."
fi
```

Tässä esimerkissä, jos hakemisto "Kuvat" on olemassa, tulostetaan "Hakemisto 'Kuvat' on olemassa." Muussa tapauksessa tulostetaan "Hakemistoa ei löytynyt."

## Syvempää tietoa:

Komento "test" tarkistaa myös muiden tiedostojen ja kansioiden olemassaolon käyttämällä erilaisia tunnuksia. Esimerkiksi "-f" tarkoittaa regular file (tavallinen tiedosto), "-e" tarkoittaa existence (olemassaolo), jne. Voit lukea lisää erilaisista tunnuksista Bashin manuaalisivulta.

Voit myös tarkistaa hakemiston olemassaolon käyttämällä "if" -lauseketta ja "ls" -komentoa, joka listaa tiedostot ja kansiot halutussa hakemistossa. Tämä lähestymistapa voi olla hyödyllinen, jos haluat tehdä muita toimintoja, kuten tulostaa sisältö tai poistaa hakemiston, jos se on olemassa.

```
Bash
if [ -d Kuvat ]
then
    echo "Hakemisto 'Kuvat' on olemassa."
else
    echo "Hakemistoa ei löytynyt."
fi
```

## Katso myös:

- [Bash Manuali](https://www.gnu.org/software/bash/manual/bash.html)
- [Linux Commando's Bash scripting tutorial](https://linuxcommando.blogspot.com/2008/03/bash-test-command.html)
- [Tutustu Bashin ehtolausekkeisiin](https://www.tutorialspoint.com/unix/unix-basic-operators.htm)