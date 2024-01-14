---
title:    "Fish Shell: Muunna päivämäärä merkkijonoksi"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monet Fish Shell käyttäjät haluavat muuntaa päivämäärän merkkijonoksi erilaisissa ohjelmointi- tai skriptausprojekteissaan. Tämä voi olla tarpeen esimerkiksi tiedoston nimen luomisessa tai tiedoston muokkaamisen yhteydessä.

## Miten

Fish Shellilla on helppo muuntaa päivämäärä merkkijonoksi käyttämällä `date` -komennon `+%Y-%m-%d` -muotoilulippua. Tämä tuottaa päivämäärästä muodossa `vuosi-kuukausi-päivä`, esimerkiksi `2020-10-15`.

```Fish Shell
set today (date +%Y-%m-%d)
echo $today
```

Tulostus: `2020-10-15`

Voit myös halutessasi lisätä tarkemman ajanhetken `+%H%M%S` -muotoilulipulla. Tämä tuottaa päivämäärästä muodossa `vuosi-kuukausi-päivä-tunti-minuutti-sekunti`, esimerkiksi `2020-10-15-153047`.

```Fish Shell
set timestamp (date +%Y-%m-%d-%H%M%S)
echo $timestamp
```

Tulostus: `2020-10-15-153047`

## Syvempään sukellus

Fish Shellin `date` -komennossa on muitakin hyödyllisiä muotoilulippuja päivämäärän muuntamiseen merkkijonoksi. Esimerkiksi `%a` palauttaa päivän nimen lyhenteenä englanniksi, kuten `Thu` torstaille ja `%B` palauttaa kuukauden nimen englanniksi, kuten `October` lokakuulle.

```Fish Shell
set day (date +%a)
set month (date +%B)
set customDate ($day-$month-%Y)
echo $customDate
```

Tulostus: `Thu-October-2020`

Voit myös määrittää tarkemman aikavyöhykkeen käyttämällä `TZ`-ympäristömuuttujaa. Tämä on hyödyllistä esimerkiksi kansainvälisissä projekteissa, joissa on eri aikavyöhykkeillä työskenteleviä henkilöitä.

```Fish Shell
setenv TZ "Europe/Helsinki"
set timestamp (date +%Y-%m-%d-%H%M%S)
echo $timestamp
```

Tulostus: `2020-10-15-200013` (Olettaen, että aikavyöhyke on asetettu Euroopan aikavyöhykkeelle)

Syvempään tietämystä päivämäärän muuntamisesta merkkijonoksi löytyy Fish Shellin dokumentaatiosta.

## Katso myös

- Fish Shellin dokumentaatio päivämäärän muuntamisesta: https://fishshell.com/docs/current/cmds/date.html
- Aikavyöhykkeiden lista: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones