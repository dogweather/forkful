---
title:                "Kahden päivämäärän vertailu"
html_title:           "Fish Shell: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Vertaamalla kahta päivämäärää tarkoitetaan päivämäärien välillä tapahtuvaa vertailua, jossa selvitetään kumpi päivämäärä on suurempi tai onko ne yhtä suuret. Tämä on tärkeää ohjelmoinnissa esimerkiksi järjestämisen ja aikaleimojen tarkastelun kannalta.

## Ohjeet:
[https://fishshell.com/docs/current/index.html]
Voit verrata kahta päivämäärää käyttämällä Fish Shell -ohjelman `test`-komentoa yhdessä `-nt`-tunnisteen kanssa. Tässä on esimerkkejä ja tulosteet:

```Fish Shell ... 
test 20191205 -nt 20191101
true
```

```Fish Shell ... 
test 20191010 -nt 20191101
false
```

## Syventävä tieto:
Päivämäärien vertailu ei ole uusi asia, vaan se on ollut oleellinen osa ohjelmointia jo pitkään. Fish Shell:n lisäksi myös muut komentokehotteiden ohjelmat, kuten Bash ja Zsh, tarjoavat mahdollisuuden päivämäärien vertailuun. Toinen tapa verrata päivämääriä on käyttää `date`-komentoa yhdessä `+%s`-parametrin kanssa, jolloin saadaan päivämäärien Unix-aikaleimat ja voidaan verrata niitä keskenään.

## Katso myös:
- [https://fishshell.com/docs/current/commands.html#test]
- [https://www.ibm.com/support/knowledgecenter/en/ssw_aix_72/com.ibm.aix.cmds1/test.htm]
- [https://www.computerhope.com/unix/udate.htm]