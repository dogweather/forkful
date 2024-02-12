---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-02-03T19:09:30.155839-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nykyisen päivämäärän hankkiminen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Nykyisen päivämäärän saaminen ohjelmoinnissa on perustavaa laatua oleva tehtävä, joka mahdollistaa järjestelmän päivämäärä- ja aikatietojen noutamisen ja manipuloinnin. Käsikirjoituksessa ja automatisoinnissa päivämäärän saaminen on välttämätöntä aikaleimojen luomiseen, tehtävien ajoittamiseen ja lokiin kirjoittamiseen.

## Miten:
Fish Shell käyttää ulkoisia komentoja, kuten `date`, nykyisen päivämäärän saamiseksi, tarjoten joustavuutta tulosteen muotoiluun tarpeen mukaan. Näin se tehdään:

```fish
# Näytä nykyinen päivämäärä oletusmuodossa
echo (date)

# Tulosteesimerkki: Kesk 25 Loka 2023 15:42:03 BST
```

Päivämäärän muodon mukauttamiseksi voit käyttää `+`-vaihtoehtoa, jonka jälkeen tulevat muotoiluspesifikaattorit:

```fish
# Näytä nykyinen päivämäärä YYYY-MM-DD muodossa
echo (date "+%Y-%m-%d")

# Tulosteesimerkki: 2023-10-25
```

Monimutkaisempiin tehtäviin, kuten aikaleimojen kanssa työskentelyyn tai päivämääräaritmetiikan suorittamiseen, Fish Shell luottaa ulkoisiin työkaluihin kuten `date`, johtuen sen skriptausluonteesta. Tässä on esimerkki nykyisen UNIX-aikaleiman saamisesta:

```fish
# Hae nykyinen UNIX-aikaleima
echo (date "+%s")

# Tulosteesimerkki: 1666710123
```

Ja lisätäksesi yhden päivän nykyiseen päivämäärään käyttämällä `date`:

```fish
# Lisää yksi päivä nykyiseen päivämäärään
echo (date -d "+1 day" "+%Y-%m-%d")

# Tulosteesimerkki: 2023-10-26
```

Huomautus: Esimerkit käyttävät `date`-komennon vaihtoehtoja, jotka toimivat GNU coreutilsin kanssa. Vaihtoehdot voivat vaihdella muissa ympäristöissä, kuten macOS:ssä, joka käyttää oletuksena BSD:n date-komentoa. Viittaa aina `date --help`-ohjeeseen tai manual-sivuun yksityiskohtia varten omassa ympäristössäsi.
