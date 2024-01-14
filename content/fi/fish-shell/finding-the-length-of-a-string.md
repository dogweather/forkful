---
title:    "Fish Shell: Merkkijonon pituuden etsiminen"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit selvittää merkkijonon pituuden? Se voi olla hyödyllistä, kun haluat tarkistaa, että syöte vastaa odotettua pituutta tai haluat suorittaa tiettyjä toimintoja merkkijonon tietyn määrän merkkejä sisältä.

## Kuinka

Fish Shell tarjoaa helpon tavan selvittää merkkijonon pituuden käyttämällä **string length** -toimintoa. Tässä on muutama esimerkki ja niiden tuottama tulos:

```Fish Shell
string length "Hei!" 
# Output: 4 
```
```Fish Shell
set string "Tämä on pitkä merkkijono" 
string length $string 
# Output: 24 
```
```Fish Shell
set input (read) 
string length $input 
```

## Syvemmälle

Fish Shellin **string length** -toiminto laskee jokaisen merkin mukaan lukien myös välilyönnit ja erikoismerkit. Lisäksi se osaa huomioida myös Unicode-merkit. Näin ollen se tuottaa tarkan luvun merkkijonon pituudesta.

## Katso myös

- Fish Shellin virallinen dokumentaatio merkkijonojen käsittelystä: https://fishshell.com/docs/current/cmds/set.html#string-operations
- Lyhyt opas Fish Shellin käyttöönottoon: https://www.linuxjournal.com/content/we-all-fish-shell-now
- Fish Shellin kieliohjeet: https://fishshell.com/docs/current/tutorial.html