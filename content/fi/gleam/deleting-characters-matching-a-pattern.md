---
title:    "Gleam: Mallien mukaan vastaavien merkkien poistaminen."
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi poistaisit merkkejä, jotka vastaavat tiettyä mallia? Tämä voi olla hyödyllistä, jos haluat puhdistaa tekstistä pois tarpeettomia merkkejä, kuten symbolit tai välimerkit. Se voi myös auttaa sinua muotoilemaan tiettyä tietoa haluamallasi tavalla.

## Miten

```Gleam
my_string = "Tervetuloa Gleam blogiin!"
print(my_string |> String.replace(@regex("[A-Z]"), ""))
```
Tämä koodi poistaa kaikki kirjaimet @regex([A-Z]), jättäen jäljelle vain pienet kirjaimet ja tulostaa "ervetuloa lei blogiin!". Tämä koodinpätkä käyttää Gleamin built-in String.replace() -funktiota, ja voit käyttää myös muita regex-sääntöjä määrittämään, mitä merkkejä poistat tekstistä.

## Syväsukellus

Regex eli regular expression on tapa ilmaista tiettyjä merkkijonon malleja. Gleam tukee regexin käyttöä String-moduulissa sen sisäänrakennetulla @regex-makrolla. Mikä tekee Gleamista erityisen hyvän vapaamuotoisiin Regular Expressionien käyttöön verrattuna, on Gleamin kyky muuntaa säännöt puhtaaksi merkkijonoksi, mikä vähentää mahdollisia virheitä syntaktisen analyysin aikana.

## Katso myös

- [Gleamin virallinen dokumentaatio regexeihin](https://gleam.run/core/Regex.html)
- [Säännölliset lausekkeet: 10 asiaa, jotka sinun pitäisi tietää](https://opensource.com/article/19/1/regular-expressions-cheat-sheet)
- [Gleamin Slack-kanava, jossa voit kysyä lisäkysymyksiä regexeihin liittyen](https://gleam-lang.slack.com/)