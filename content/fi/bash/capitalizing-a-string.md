---
title:    "Bash: Merkkijonon ensimmäisen kirjaimen suurennus"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon isot kirjaimet?

## Kuinka tehdä se

On monia tapoja muuttaa merkkijonon isot kirjaimet Bash-skriptissä. Yksi tapa on käyttää sisäänrakennettua `tr` -toimintoa.

```
Bash```
# Luodaan muuttuja, jossa on merkkijono
string="tervetuloa suomeen"
# Käytetään 'tr' muuttamaan kaikki kirjaimet isoksi kirjaimiksi
new_string="$(echo $string | tr [a-z] [A-Z])"
# Tulostetaan uusi merkkijono
echo $new_string
```

Tuloste:
```
TERVETULOA SUOMEEN
```

On myös mahdollista käyttää `awk` komentoa muuttaa merkkijonon isot kirjaimet. Alla olevassa esimerkissä käytämme `toupper` -toimintoa muuttaaksemme merkkijonon isot kirjaimet.

```
Bash```
# Luodaan muuttuja, jossa on merkkijono
string="tervetuloa suomeen"
# Käytetään 'awk' muuttamaan kaikki kirjaimet isoksi kirjaimiksi
new_string="$(echo $string | awk '{print toupper($0)}')"
# Tulostetaan uusi merkkijono
echo $new_string
```

Tuloste:
```
TERVETULOA SUOMEEN
```

## Syvenny

Molemmat edellä mainitut esimerkit käyttävät sisäänrakennettuja funktioita muuttaakseen merkkijonon isot kirjaimet. `tr` -toiminto käyttää ASCII-merkkejä muuttaakseen kirjaimet, kun taas `toupper` -toiminto käyttää C-kielen kirjastofunktiota muuttaakseen merkkijonon isot kirjaimet. Syvennä tutkimalla näitä funktioita ja niiden tarkoitusta.

## Katso myös

* `tr` -toiminnon dokumentaatio: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
* `toupper` -toiminnon dokumentaatio: https://www.gnu.org/software/gawk/manual/html_node/Using-the-Function-toupper.html