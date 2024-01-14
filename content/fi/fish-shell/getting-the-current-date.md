---
title:    "Fish Shell: Nykyisen päivämäärän hankkiminen"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
Mikä voi olla syy siihen, miksi haluaisit saada nykyisen päivämäärän Fish Shellissä? On monia tilanteita, joissa tämä voi olla hyödyllistä, kuten päivämäärän lisääminen tiedostojen nimiin tai skriptien luomiseen, jotka käyttävät päivämäärää osana prosesseja.

## Kuinka
```Fish Shell``` -koodilohkojen avulla voit helposti saada nykyisen päivämäärän ja tulostaa sen komentokehotteeseen. Alla olevassa esimerkissä käytämme ```date``` -komennon ```+%D``` -valitsinta, joka näyttää päivämäärän muodossa ```mm/dd/vvvv```.

```Fish Shell
echo "Tänään on $(date +%D)"
```

Tämä antaisi seuraavan tulostuksen:

```
Tänään on 06/15/2021
```

## Syvenny
Fish Shellin ```date``` -komennossa on paljon muita saatavilla olevia muotoiluvaihtoehtoja. Esimerkiksi voit lisätä kellonajan antamalla ```+%r``` vaihtoehdon:

```Fish Shell
echo "Kello on $(date +%r)"
```

Tämä tuottaisi tuloksen:

```
Kello on 08:15:32 PM
```

Voit myös muokata päivämäärän muotoa antamalla tarvittavan muotoilumerkin. Esimerkiksi ```+%A``` antaisi viikonpäivän nimen, kun taas ```+%B``` antaisi kuukauden nimen.

## Katso myös
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell Date Command Documentation](https://fishshell.com/docs/current/cmds/date.html)