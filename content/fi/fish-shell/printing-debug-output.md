---
title:                "Tulostaminen virheselvitystulosteena"
html_title:           "Fish Shell: Tulostaminen virheselvitystulosteena"
simple_title:         "Tulostaminen virheselvitystulosteena"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi
Miksi tulostaa vianmääritystiedot? Se on yksi helpoimmista tavoista tarkistaa koodin suorittamista ja tunnistaa ongelmia.

## Miten
```Fish Shellin``` avulla voit tulostaa vianmääritystietoja käyttämällä ```echo``` komentoa. Tämä tulostaa halutun tiedon terminaaliin.

```
echo $var
```

## Syväsukellus
```echo``` komentoa voidaan käyttää monin eri tavoin vianmääritystiedon tulostamiseen. Voit esimerkiksi tulostaa muuttujan sisällön tai jopa koko tiedoston sisällön.

```
echo $var > tiedosto.txt
echo (cat tiedosto.txt)
```

Voit myös yhdistää ```echo``` ja ```sed``` komennot luodaksesi dynaamisia tulosteita.

```
echo (sed -n 5p tiedosto.txt)
```

## Katso myös
Tässä muutamia hyödyllisiä linkkejä lisätietoa ja esimerkkejä varten:

- Fish Shellin kotisivu: https://fishshell.com/
- Fish Shellin dokumentaatio: https://fishshell.com/docs/current/
- Fish Shellin keskustelufoorumi: https://fishshell.com/docs/current/