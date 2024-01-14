---
title:                "Fish Shell: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Yksi yleinen ohjelmoinnin tehtävä on poistaa tietyllä kaavalla vastaavat merkit tekstistä. Tämä voi olla hyödyllistä, kun halutaan nopeasti muokata tai puhdistaa tietoja.

## Miten

Fish Shellilla tämä tehtävä onnistuu helposti käyttämällä `sed`-komentoa yhdessä regex-säännön kanssa. Tässä esimerkki, jossa poistetaan kaikki numerot ja välilyönnit merkkijonosta:

````Fish Shell
echo "123 is 456 my 789 number" | sed 's/[[:digit:][:space:]]//g'
````

Tämä tuottaa seuraavan tulosteen:

```
is my number
```

Vaihtoehtoisesti voit myös käyttää `tr`-komennon avulla poistamaan halutut merkit:

````Fish Shell
echo "123 is 456 my 789 number" | tr -d '[0-9[:space:]]'
````

Tämä tuottaa saman tuloksen kuin edellinen esimerkki.

## Syvällinen tarkastelu

Tässä syvällisempiä tietoja regex-säännöistä ja niiden käytöstä Fish Shellissa.

Regex on lyhenne sanoista regular expression, eli suomeksi säännöllinen lauseke. Se on merkkijono, joka määrittelee tietyn kaavan, jota vastaavat merkit poistetaan tai korvataan annetusta tekstistä. Fish Shellissa voi käyttää useita erilaisia regex-sääntöjä, joilla voi poistaa erilaisia merkkejä tai merkkiyhdistelmiä, kuten numeroita, välilyöntejä, kirjaimia tai erikoismerkkejä.

## Katso myös

- [Fish Shellin dokumentaatio](https://fishshell.com/docs/current/index.html#regex)
- [Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
- [Säännöllisten lausekkeiden opas](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)