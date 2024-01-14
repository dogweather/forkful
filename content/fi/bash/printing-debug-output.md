---
title:                "Bash: Tulostamalla debuggaustulosteita"
simple_title:         "Tulostamalla debuggaustulosteita"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

# Miksi tulostaa virheenkorjaustiedot?

Virheenkorjaustietojen tulostaminen on tärkeä osa ohjelmointia ja auttaa kehittäjiä löytämään ja korjaamaan ohjelmien virheitä. Se voi myös auttaa ymmärtämään ohjelman suorituksen aikana tapahtuvia prosesseja ja löytämään mahdollisia optimointimahdollisuuksia.

# Miten tulostaa virheenkorjaustiedot?

Virheenkorjaustietojen tulostaminen Bash-ohjelmassa on helppoa. Tulostus tapahtuu käyttämällä `echo`-komentoa ja lisäämällä siihen halutut muuttujat tai merkkijonot. Alla on esimerkkikoodi ja sen tulostama tulos:

```Bash
#!/bin/bash
nimi="Matti"
lause="Terve $nimi, mitä kuuluu?"
echo $lause
```

Tämä koodi tulostaa seuraavan rivin:

```
Terve Matti, mitä kuuluu?
```

## Syvä sukellus

Bashilla on mahdollista myös tulostaa virheenkorjaustietoja eri tasoisilla painikkeilla, kuten `-v` ja `-x`. Nämä painikkeet auttavat seuraamaan ohjelman suoritusta tarkemmin ja löytämään mahdollisia ongelmakohtia.

On myös hyödyllistä lisätä `echo`-lauseita ohjelman eri osiin, jotta voidaan seurata suorituksen kulkua ja löytää mahdollisia virheitä tai pullonkauloja. Näitä lisäyksiä kutsutaan myös nimellä "debugging prints".

## Katso myös

Tässä muutamia hyödyllisiä linkkejä, jotka voivat auttaa sinua tulostamaan virheenkorjaustietoja Bash-ohjelmissasi:

- [Bashin opas tulostamiseen](https://www.tutorialspoint.com/unix_commands/echo.htm)
- [Tämä Stack Overflow-kysymys selittää, miten tulostaa painikkeiden avulla](https://stackoverflow.com/questions/1682661/how-can-i-echo-a-4-digit-code-in-a-while-loop-in-the-unix-shell)
- [Debuggausvinkkejä Bash-ohjelmistoille](https://dev.to/tanwanimohit/debugging-tips-for-bash-programming-13lp)