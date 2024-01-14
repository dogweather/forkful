---
title:    "Fish Shell: Säännöllisten lausekkeiden käyttö"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita ohjelmoinnissa?

Säännölliset lausekkeet ovat voimakas työkalu, jota voidaan käyttää tiettyjen merkkijonojen etsimiseen ja korvaamiseen suuresta tekstiaineistosta. Ne ovat erityisen hyödyllisiä, kun käsitellään suuria määriä tietoa, koska ne säästävät aikaa ja vaivaa manuaalisesti suoritettavilta toiminnoilta. 
Säännölliset lausekkeet ovat myös olennainen osa monia ohjelmointikieliä, kuten Fish Shell, joten niiden käyttöä on tärkeää oppia.

## Näin käytät säännöllisiä lausekkeita Fish Shellissä

Fish Shell tarjoaa kätevän ja tehokkaan tavan käyttää säännöllisiä lausekkeita. Ensimmäinen askel on selvittää, mitä haluat etsiä ja korvata. Tämän jälkeen voit seurata näitä ohjeita:

1. Avaa Fish Shell ja siirry hakemistoon, jossa haluat suorittaa ohjelmointisi.
2. Kirjoita "```grep [etsittävä lauseke] [tiedosto]```", jossa [etsittävä lauseke] on haluamasi lauseke ja [tiedosto] on tiedosto, johon haluat etsiä. Tämä tulostaa kaikki rivit, joissa löytyy haluamasi lauseke.
3. Jos haluat korvata lausekkeen, lisää "```-E 's/[vanha lauseke]/[uusi lauseke]'```" komentoriville. Tämä korvaa vanhan lausekkeen uudella lausekkeella kaikissa tuloksissa.
4. Voit myös lisätä "```-i```" komentoriville, jos haluat tehdä korvauksen isojen ja pienten kirjainten välillä välittämättä.

Jos haluat esimerkin, jossa halutaan etsiä ja korvata kaikki "hello everyone" -lausekkeet "hello world" -lausekkeella tiedostosta "tervehdys.txt", komentorivisi olisi seuraavanlainen:

```fish
grep "hello everyone" tervehdys.txt-E 's/hello everyone/hello world/' -i
```

Tämä tulostaisi kaikki rivit, joissa löytyy "hello everyone" -lauseke, mutta korvaisi sen "hello world" -lausekkeella.

## Syvempi sukellus säännöllisiin lausekkeisiin

Vaikka simple syntax regexes ovat hyvin hyödyllisiä ja yleensä toimivat useimmissa tapauksissa, säännölliset lausekkeet ovat paljon monipuolisempia ja monimutkaisempia kuin mitä tässä blogikirjoituksessa kuvataan. Jos haluat oppia enemmän, voit tutustua esimerkiksi seuraaviin sivustoihin:

- [fishshell.com/docs/current/cmds/grep.html](https://fishshell.com/docs/current/cmds/grep.html)
- [regex.info/](http://regex.info/)
- [regexone.com/](https://regexone.com/)
- [en.wikipedia.org/wiki/Regular_expression](https://en.wikipedia.org/wiki/Regular_expression)

## Katso myös

- [fishshell.com](https://fishshell.com/)
- [fishshell.com/docs/](https://fishshell.com/docs/)
- [github.com/fish-shell/fish-shell](https://github.com/fish-shell/fish-shell)
- [stackoverflow.com/questions/tagged/fish+regex](https://stackoverflow.com/questions/tagged/fish+regex)