---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:38:14.324467-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Päivämäärän jäsentäminen merkkijonosta tarkoittaa päivämäärämuotoisen tekstin muuttamista ohjelmoitavaksi objektiksi. Tämän teemme, koska haluamme käsitellä päivää ja aikaa ohjelmassamme – esimerkiksi tallentaa tietokantaan, vertailla päivämääriä tai muotoilla ne uudelleen.

## How to: (Kuinka tehdä:)
```Python
from datetime import datetime

# Syötetty päivämäärä merkkijonona
date_string = "28.2.2023"

# Määritellään päivämäärän muoto
format = "%d.%m.%Y"

# Jäsentäminen datetime-objektiksi
parsed_date = datetime.strptime(date_string, format)

# Tulostetaan jäsentynyt päivämäärä
print(parsed_date)
# Output: 2023-02-28 00:00:00

# Lisäesimerkki: muot laajennettu aikaan asti
time_string = "28.2.2023 14:45"
format_with_time = "%d.%m.%Y %H:%M"
parsed_time = datetime.strptime(time_string, format_with_time)
print(parsed_time)
# Output: 2023-02-28 14:45:00
```

## Deep Dive (Syväsukellus):
Päivämäärien jäsentäminen on ollut ohjelmoinnissa alusta asti, sillä päivämäärien kanssa työskentely on universaali vaatimus. Historiallisesti päivämääriä on käsitelty monin eri tavoin riippuen kielestä ja alustasta. Pythonissa `datetime`-kirjastoa on käytetty laajalti alkuvuosista lähtien päivämäärien jäsentämiseen. 

Vaihtoehtoina `datetime`-moduulille voisi käyttää kolmannen osapuolen kirjastoja kuten `dateutil`, joka tarjoaa joustavampia työkaluja päivämäärien jäsentämiseen. Esimerkiksi, `dateutil` sietää erilaisia päivämäärämuotoja paremmin.

Jäsentämisen yksityiskohdat ovat tärkeitä; väärä muoto tai virhe tulkitessa voi johtaa vääriin tuloksiin tai virheisiin. Käyttäessä `strptime`-metodia muista, että sinun tulee määritellä täsmälleen oikea muoto, jotta jäsentäminen onnistuu.

## See Also (Katso myös):
- Pythonin virallinen dokumentaatio datetime-moduulista: https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior
- Dateutil-kirjaston kotisivu: https://dateutil.readthedocs.io/en/stable/
- Pythonin aikavyöhykeiden käsittelyyn pytz-kirjasto: http://pytz.sourceforge.net/
