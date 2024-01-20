---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Fish Shell: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Laskeminen päivämäärä tulevaisuudessa tai menneisyydessä yksinkertaisesti tarkoittaa päivämäärän muokkaamista nykyhetkestä tietyllä määrällä päiviä tai kuukausia joko eteen- tai taaksepäin. Ohjelmoijat tekevät tämän esimerkiksi arvioimaan projektin kestoajat, määrittelmään julkaisupäivämään tai pitämään kirjaa viimeisestä päivityksestä.

## Näin teet:

```Fish Shell
# Tulevaisuuden päivämäärä
set -l future_date (date -d "+30 days" +"%d.%m.%Y")
echo $future_date
```
Sampla ulostulo esimerkiksi:

```28.11.2022```

```Fish Shell
# Menneisyyden päivämäärä
set -l past_date (date -d "-30 days" +"%d.%m.%Y")
echo $past_date
```
Sampla ulostulo esimerkiksi:

```29.09.2022```
    

## Syvä sukellus

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä on yksinkertainen, mutta välttämätön komento ohjelmoinnin maailmassa. Tämä päivämääräkäsittelykonsepti otettiin käyttöön jo varhaisina UNIX-aikoina ja on osa perusohjelmointia. 

Fish Shell tarjoaa tehokkaan ja yksinkertaisen tavan käsitellä aikaa, mutta muita vaihtoehtoisia shell-kieliä, kuten Zsh tai Bash, voidaan myös käyttää. Implementointi määrittyy suurelta osin 'date' -komennon avulla, joka tulkitsee tekstiä aikaa kuvaaviksi tyypeiksi.

## Katso myös:

[Fish Shell Documentaatio:](https://fishshell.com/docs/)

[UNIX ajanlaskun historia:](https://www.eecis.udel.edu/~mills/leap.html)

[Käytä Bash keskustelun hallintaan:](https://www.linuxjournal.com/content/using-bash-dialogs)