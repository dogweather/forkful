---
title:                "Tekstin kirjoittaminen isolla alkukirjaimella"
html_title:           "Fish Shell: Tekstin kirjoittaminen isolla alkukirjaimella"
simple_title:         "Tekstin kirjoittaminen isolla alkukirjaimella"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Stringin muuttaminen isolla alkukirjaimella voi olla hyödyllistä esimerkiksi silloin, kun halutaan korostaa jonkin asian merkitystä tai tehdä siitä erottuvampi.

## Kuinka tehdä

Capitalization onnistuu helposti käyttämällä fish shellin sisäänrakennettua `string` komentoa. Alla on esimerkkejä erilaisista tapauksista:

```Fish Shell
# Ensimmäisen kirjaimen muuttaminen isoksi
set string "fish shell"
echo (string capitalize $string) # Tulostaa "Fish shell"

# Kaikkien kirjainten muuttaminen isoiksi
set string "fish shell"
echo (string capitalize -a $string) # Tulostaa "FISH SHELL"

# Sanojen ensimmäisten kirjainten muuttaminen isoiksi
set string "fish shell"
echo (string capitalize -w $string) # Tulostaa "Fish Shell"

# Lisäämällä rivinvaihtoja stringiin
set string "fish\nshell"
echo (string capitalize -wl $string) # Tulostaa "Fish\nShell"
```

## Syvemmälle

`string` komennolla on myös muita hyödyllisiä vaihtoehtoja, kuten muuttaminen pieniksi kirjaimiksi (`-l`), muuttaminen isoiksi kirjaimiksi mutta säilyttäen muut kirjaimet samoina (`-uc`) tai muuttaminen camel case muotoon (`-c`). Lisätietoja näistä ja muista vaihtoehdoista löytyy fish shellin dokumentaatiosta.

## Katso myös

- [Fish Shell dokumentaatio](https://fishshell.com/docs/current/)
- [String-komennon dokumentaatio](https://fishshell.com/docs/current/cmds/string.html)