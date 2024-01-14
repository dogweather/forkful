---
title:    "Fish Shell: Testien kirjoittaminen"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

# Miksi: Kirjoittamalla testejä
Kirjoittamalla testejä voi varmistaa koodin toimivuuden ja vähentää mahdollisia virheitä. Testien avulla voi myös helpommin muokata ja päivittää koodia ilman pelkoa siitä, että se aiheuttaa ongelmia muualla.

## Miten:
Harjoitellaan testien kirjoittamista Fish Shellin avulla.

```Fish Shell
echo "Tervetuloa Fish Shell blogiin!"
sleep 1
```

```Fish Shell
set name "Maija"
echo "Hei " $name
sleep 1
```

Tässä esimerkissä luodaan muuttuja "name" nimelle "Maija" ja printataan tervehdys hänen nimellään. "sleep 1" komennolla viive simuloituna, jotta koodi näyttää hitaammalta testien aikana.

## Syvemmälle:
Testien kirjoittamiseen on useita erilaisia lähestymistapoja, mutta yleisesti ottaen tärkeintä on varmistaa, että ne testaavat kaikkia koodin osia ja mahdollisia virhetilanteita. Tärkeää on myös pitää testit jatkuvasti päivitettyinä muutosten yhteydessä.

On myös tärkeää käyttää erilaisia testaustyökaluja, kuten käyttöliittymätestauksia ja yksikkötestejä, jotta koodin laatu ja toimivuus voidaan taata.

## Katso myös:
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell Test Frameworks](https://gist.github.com/fisherman/on-the-fly-1f7677c3f3b81f17f9c59200bb928ae3)
- [How to Write Good Tests](https://www.adventuresintechland.com/writing-good-unit-tests/)