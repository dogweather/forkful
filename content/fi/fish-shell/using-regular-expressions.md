---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
simple_title:         "Säännöllisten lausekkeiden käyttö"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Regulaarilausekkeet on työkaluja tekstijonon kuvailuun ja manipulointiin. Ohjelmoijat käyttävät niitä, koska ne tarjoavat voimakkaan tavan etsiä, korvata ja analysoida tekstitietoja nopeasti.

## How to:
```Fish Shell
# Esimerkki: Etsi kaikki 'fish' sanaa vastaavat merkkijonot
echo "fish are friends, not food." | string match -r 'fish'
```
Output:
```
fish
```

```Fish Shell
# Esimerkki: Korvaa 'fish' sanalla 'shark'
echo "fish are friends, not food." | string replace 'fish' 'shark'
```
Output:
```
shark are friends, not food.
```

```Fish Shell
# Esimerkki: Etsi kaikki numerot tekstistä
echo "I have 2 fish and 1 dog" | string match -r '[0-9]+'
```
Output:
```
2
1
```

## Deep Dive
Regulaarilausekkeet, eli regex, syntyi 1950-luvulla Stephen Kleenen kehittämänä teoreettisena konseptina. Vaihtoehtona regex-käytölle voidaan käyttää käsitteleviä kirjastoja tai kielen omia tekstin käsittelykykyjä. Fish Shellissä regex-tuki tulee komennosta `string`, joka sisältää alikomennon `match` hakemista varten ja `replace` korvaamista varten, sekä muita.

## See Also
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [GNU Grep Manual](https://www.gnu.org/software/grep/manual/grep.html)
