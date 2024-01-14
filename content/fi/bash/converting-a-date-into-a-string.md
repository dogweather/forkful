---
title:    "Bash: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

#"## Miksi"

Tervetuloa blogiimme, jossa kerromme kuinka muuttaa päivämäärä merkkijonoksi käyttäen Bash-ohjelmointia. Tässä artikkelissa käymme läpi, miksi tämä prosessi olisi hyödyllinen ja annamme käytännön esimerkkejä sen toteuttamiseen.

#"## Miten tehdä"

Muuttaminen päivämäärä merkkijonoksi voi olla hyödyllistä monissa ohjelmointiprojekteissa. Se voi auttaa visualisoimaan päivämääriä selkeämmin, helpottaa tiedostojen nimeämistä ja tehdä päivämäärän käsittelemisestä yleisesti ottaen helpompaa. Alla esittelemme, kuinka voit tehdä tämän Bash-ohjelmointikielellä käyttämällä muutamia käteviä komentoja.

```Bash
# Asetetaan päivämäärä muuttujaan
date=2021-08-15

# Muutetaan päivämäärä merkkijonoksi käyttämällä "date" komentoa
string_date=$(date -d $date +"%d/%m/%Y")

# Tulostetaan muutettu merkkijono
echo $string_date

```

Tämä esimerkki muuntaa päivämäärän formaattiin "päivä/kuukausi/vuosi" ja tulostaa sen terminaaliin. Voit muokata komentoja, jotta ne vastaavat omaa tarvettasi. Esimerkiksi voit muuttaa päivämäärän formaattia tai lisätä siihen ajan.

#"## Syvemmälle"

Kun muutat päivämäärän merkkijonoksi Bashissa, käytät todennäköisesti "date" komentoa ja sen avulla saatavilla olevia muotoilumerkkejä. Voit esimerkiksi muuttaa päivämäärää käyttäen "d" merkkiä, jota seuraa merkkijono, joka määrittää halutun formaatin. Alla on muutamia esimerkkejä näistä merkkijonoista ja niiden tulostamista formaateista:

- %a: päivän nimi lyhyessä muodossa (esim. ma, ti)
- %A: päivän nimi pitkässä muodossa (esim. maanantai, tiistai)
- %b: kuukauden nimi lyhyessä muodossa (esim. tammi, helmi)
- %B: kauden nimi pitkässä muodossa (esim. tammikuu, helmikuu)
- %m: kuukauden numero kahdella numerolla (esim. 08)
- %d: päivämäärän numero kahdella numerolla (esim. 15)
- %Y: vuosiluku neljällä numerolla (esim. 2021)
- %T: aika 24-tuntisena kelloaikana (esim. 15:30:00)

Voit käyttää näitä merkkejä yhdistämällä ne haluamallasi tavalla saadaksesi halutun muodon.

#"## Katso myös"

- Bashin "date" komennon dokumentaatio: https://www.gnu.org/software/coreutils/manual/html_node/Date-conversion-specifiers.html
- Toisenlaisia Bashin päivämäärän muuntoesimerkkejä: https://bash.cyberciti.biz/guide/Relational_operators
- Bashin Python-yhteysohjelman avulla voit tehdä päivämäärän muunto-operaatioita: https://github.com/eliben/code-for-blog/blob/master/2018/python-bash-bench/pipe_cmd_to_py_bench.sh