---
date: 2024-01-20 17:35:46.690789-07:00
description: "How to: Bash sis\xE4lt\xE4\xE4 `date`-komennon p\xE4iv\xE4m\xE4\xE4\
  r\xE4n k\xE4sittelyyn, mukaan lukien muunnokset merkkijonoiksi. Historiallisesti\
  \ Unix-j\xE4rjestelmiss\xE4 p\xE4iv\xE4m\xE4\xE4r\xE4n\u2026"
lastmod: '2024-04-05T21:53:58.325249-06:00'
model: gpt-4-1106-preview
summary: "Bash sis\xE4lt\xE4\xE4 `date`-komennon p\xE4iv\xE4m\xE4\xE4r\xE4n k\xE4\
  sittelyyn, mukaan lukien muunnokset merkkijonoiksi."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

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
