---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Alimerkkijonon purkaminen on prosessi, jossa otetaan osa merkkijonosta käyttämällä sen indeksejä. Ohjelmoijat tekevät tämän tiedon erottamiseksi tai muokkaamiseksi.

## Kuinka tehdään:

Tässä on esimerkki siitä, miten alimerkkijono voidaan hakea Bash:lla:

```Bash
string="Tervetuloa Bash ohjelmointiin."
substring=${string:11:4}
echo $substring
```

Tässä, `string:11:4` ottaa 4 merkkiä merkkijonosta alkaen 11. indeksistä. Tuloste:

```Bash
Bash
```

## Syvempi tarkastelu:

Historiallisesti Unix-komentorivikielet, kuten Bash, sisälsivät alimerkkijonokäsittelyn jo varhain. Vaihtoehtoja alimerkkijonojen käsittelyyn ovat esimerkiksi `cut`, `awk` ja `perl`-komentoriviohjelmat. Bashin alimerkkijonojen purkamisen implementointi on tehty C-koodauksella, joten se on suoraviivaista ja nopeaa.

## Lisätietoja:

Bash-programmoinnin syvällisempään oppimiseen suosittelemme seuraavia lähteitä:

1. GNU Bash-ohje: [http://www.gnu.org/software/bash/manual/bash.html](http://www.gnu.org/software/bash/manual/bash.html)
2. Cut-Komennon Käyttö: [https://linuxize.com/post/how-to-use-cut-command-in-linux/](https://linuxize.com/post/how-to-use-cut-command-in-linux/)
3. Bash String Manipulations: [https://tldp.org/LDP/abs/html/string-manipulation.html](https://tldp.org/LDP/abs/html/string-manipulation.html)