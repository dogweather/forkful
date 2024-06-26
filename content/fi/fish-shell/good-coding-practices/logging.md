---
date: 2024-01-26 01:02:56.950758-07:00
description: "Miten: Fishiss\xE4 lokitus voi olla yksinkertaista kuin standarditulosteiden\
  \ ja -virheiden ohjaaminen tiedostoon. Tehd\xE4\xE4n lokimerkint\xE4 skriptimme\
  \ aloitus- ja\u2026"
lastmod: '2024-03-13T22:44:57.001337-06:00'
model: gpt-4-1106-preview
summary: "Fishiss\xE4 lokitus voi olla yksinkertaista kuin standarditulosteiden ja\
  \ -virheiden ohjaaminen tiedostoon."
title: Lokitus
weight: 17
---

## Miten:
Fishissä lokitus voi olla yksinkertaista kuin standarditulosteiden ja -virheiden ohjaaminen tiedostoon. Tehdään lokimerkintä skriptimme aloitus- ja lopetusaikoihin.

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - Skripti alkoi" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - Skripti päättyi" >> my_app.log
end

log_start
# ... skriptisi tehtävät ...
log_end

cat my_app.log
```

Tältä näyttäisi `my_app.log`-tiedostossa:

```
2023-04-01 10:35:47  - Skripti alkoi
2023-04-01 10:36:02  - Skripti päättyi
```

Edistyneempään lokitukseen voit käyttää funktioita, joilla on parametrit lokitasolle ja viesteille:

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "Tämä on informaatioviesti."
log_message ERROR "Jotain meni pieleen!"
```

Näyte `my_app.log`-tulosteesta on:
```
2023-04-01 10:35:47 [INFO] Tämä on informaatioviesti.
2023-04-01 10:35:49 [ERROR] Jotain meni pieleen!
```

## Syväsukellus
Historiallisesti lokitusta shell-skripteissä on toteutettu joukolla `echo`-komentoja, ja vaikka tämä on edelleenkin vaihtoehto, monimutkaisempien järjestelmien toteuttaminen voi olla haasteellista. Fishillä ei ole sisäänrakennettua lokitusmekanismia kuten joillakin muilla kuorilla tai ohjelmointikielillä, joten usein sinun täytyy toteuttaa omat ratkaisusi.

Vaihtoehtoja Fishin sisäänrakennetulle `echo`-komennolle lokitukseen sisältävät Unix-työkalut kuten `syslog` tai `logger`, jotka yhdistävät järjestelmän lokidaemoniin, tarjoten integroidumman lähestymistavan järjestelmänlaajuiseen lokitukseen.

Fishin yksinkertaisuus mahdollistaa funktioiden luomisen lokituksen verbaalisuuden käsittelyyn, asettamalla eri tasoja, joita voit kytkeä päälle tai pois. Jotkin toteutukset voivat sisältää jopa skriptin nimen, rivinumeron ja aikaleiman, mikä helpottaa tapahtumiin johtaneiden vaiheiden jäljitettävyyttä.

## Katso myös
- Fish-kuoren dokumentaatio funktioiden kirjoittamisesta: https://fishshell.com/docs/current/#syntax-function
- Vinkkejä perusshell-skriptaukseen: https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Opas Syslog-protokollasta: https://tools.ietf.org/html/rfc5424
