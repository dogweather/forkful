---
title:                "Päivämäärän muuntaminen merkkijonoksi"
date:                  2024-01-20T17:35:46.690789-07:00
model:                 gpt-4-1106-preview
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Muunnetaan päivämäärä merkkijonoksi, koska jäsennellyt formaatit on helpompi jakaa ja käsitellä. Joskus tarvitaan tarkkaa muotoilua, esimerkiksi lokitiedostoissa tai rajapintavastauksissa.

## How to:
```Bash
# Tämänhetkinen päivä standardimuodossa
date_iso=$(date --iso-8601)
echo $date_iso
# Output esimerkiksi: 2023-04-12

# Mukautettu päivämäärän muotoilu
date_custom=$(date '+%Y-%m-%d %H:%M:%S')
echo $date_custom
# Output esimerkiksi: 2023-04-12 15:30:45
```

## Deep Dive
Bash sisältää `date`-komennon päivämäärän käsittelyyn, mukaan lukien muunnokset merkkijonoiksi. Historiallisesti Unix-järjestelmissä päivämäärän käsittely on aina ollut keskeistä, ja `date` on säilynyt tärkeänä työkaluna. Vaihtoehtoina on muita työkaluja kuten `strftime` Pythonissa, joka tarjoaa samankaltaista joustavuutta. Tarkka muotoilu tapahtuu formatointimerkkijonojen avulla, joissa esimerkiksi `%Y` edustaa nelinumeroista vuotta ja `%H:%M:%S` edustaa kelloaikaa.

## See Also
- GNU coreutils `date` manuaalisivu: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Bash Datei-käsittelytyökalut: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
- Advanced Bash-Scripting Guide – Päivämäärät ja Ajat: https://tldp.org/LDP/abs/html/dates.html
