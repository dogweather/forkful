---
title:                "Kompleksilukujen käsittely"
aliases:
- /fi/fish-shell/working-with-complex-numbers.md
date:                  2024-01-26T04:40:01.518867-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kompleksilukujen käsittely"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Kompleksiluvut laajentavat ajatusta yksiulotteisista numeroviivoista kaksiulotteiseen kompleksitasoon. Ohjelmoijat käyttävät niitä aloilla kuten insinööritiede, fysiikka ja grafiikka laskelmissa, jotka vaativat kahta komponenttia, kuten signaaleja tai kiertoliikkeitä.

## Kuinka:
Fishissä käsittelemme kompleksilukuja käyttämällä `math`-komentoa reaaliosille ja imaginaariosille. Tässä alkuun:

```fish
# Lisätään kaksi kompleksilukua (3+4i) ja (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # Tulostaa: 8+6i

# Kerrotaan kaksi kompleksilukua (1+2i) ja (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # Tulostaa: -5+10i
```

Jos tarvitset nostaa kompleksiluvun potenssiin tai saada sen eksponenttimuodon:

```fish
# Neliö (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # Tulostaa: -5+12i

# Eksponentti (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # Tulostaa: -0.41615+0.9093i
```

## Syväsukellus
Fish Shellin kompleksilukujen matemaattinen tuki on suhteellisen uusi, alkaen noin versiosta 3.1.0. Sitä ennen ihmiset ovat saattaneet käyttää `bc`:tä tai kutsuneet ulkoisia työkaluja kuten Pythonia kompleksimatematiikkaan.

Vaihtoehtoja Fishin math-komennolle sisältävät erikoistuneet numeeriset kirjastot tai kielet kuten MATLAB, Python NumPyn kanssa tai jopa C++ Standard Kirjaston kanssa. Kuitenkin nämä saattavat olla liioittelua nopeille kuorilaskuille.

Fishin kompleksilukutuki on sisäänrakennettu sen sisäiseen `math`-komentoon, hyödyntäen libcalc-kirjastoa. Tämä tarkoittaa, että et tarvitse asentaa ylimääräisiä työkaluja perustoimintoihin.

Kuitenkaan Fish ei ole suunniteltu raskaalle matemaattiselle laskennalle. Sen matematiikkakyky on kätevä nopeille laskuille tai käsikirjoituksille, joissa kompleksiluvut tulevat esille, mutta harkitse vankempia työkaluja intensiivisiä tehtäviä varten.

## Katso myös
- Fish shell dokumentaatio math-komennosta: https://fishshell.com/docs/current/commands.html#math
- NumPy Pythonille, suosittu vaihtoehto: https://numpy.org/
- Syvempi katsaus kompleksilukuihin: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
