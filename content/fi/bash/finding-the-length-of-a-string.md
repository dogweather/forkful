---
title:                "Bash: Merkkijonon pituuden selvittäminen"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Miksi
On monia syitä, miksi haluat selvittää merkkijonon pituuden. Ehkä haluat tarkistaa, että syöttämäsi tiedot ovat oikeassa muodossa, tai ehkä tarvitset merkkijonon pituuden laskemista osana suurempaa Bash-skriptiä. Joka tapauksessa, oppiminen kuinka löytää merkkijonon pituus on tärkeä taito Bash-ohjelmoinnissa.

# Kuinka tehdä se
Bashilla on valmiiksi sisäänrakennettu toiminto merkkijonon pituuden laskemiseen, `expr length`. Voit käyttää tätä toimintoa seuraavasti:

```Bash
merkkijono="Tervetuloa!"
pituus=$(expr length $merkkijono)
echo $pituus
```

Tämä tulostaa `12`, joka on `merkkijono` -muuttujan pituus. Huomaa, että `expr length` vaatii välilyöntien käyttämistä merkkijonon ja muuttujan välillä.

Voit myös käyttää toista tapaa löytää merkkijonon pituus, käyttämällä `${#muuttuja}` -komentoa:

```Bash
merkkijono="Tervetuloa!"
pituus=${#merkkijono}
echo $pituus
```

Tämä tulostaa myös `12`. Tässä metodissa ei tarvita välilyöntejä ja voit käyttää sitä suoraan määriteltyihin muuttujiin.

# Syventyminen
Bashilla on monta muuta tapaa löytää merkkijonon pituus, kuten käyttämällä `wc -c` -komennon outputia, tai käyttämällä `declare -p muuttuja` -komentoa. Voit myös käyttää regex-säännöksiä löytääksesi merkkijonon pituuden. Tärkeintä on löytää tapa, joka toimii sinun tarpeisiisi parhaiten ja joka on helposti ymmärrettävissä muille ohjelmoijille.

Katso myös:
- [Bashin String Manipulation -opas](https://www.baeldung.com/linux/string-manipulation)
- [Bashin käyttöönotto Wiki-pediassa](https://fi.wikipedia.org/wiki/Bash)
- [Bashin resurssit ja oppaat GitHubissa](https://github.com/awesome-lists/awesome-bash)