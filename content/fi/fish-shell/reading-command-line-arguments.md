---
title:    "Fish Shell: Komentoriviparametrien lukeminen"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi

Fish Shell on tehokas työkalu, joka voi auttaa sinua nopeuttamaan ja yksinkertaistamaan päivittäisiä ohjelmointitehtäviäsi. Lue eteenpäin ja opi lukemaan komentorivin argumentteja Fish Shell -ympäristössä!

## Miten

Fish Shellilla on helppo lukea komentorivin argumentteja käyttämällä “$argv” muuttujaa. Tämä muuttuja sisältää kaikki annetut komentorivin argumentit, eriteltynä välilyönneillä. Tässä on esimerkki, miten tulostaa kaikki annetut argumentit:

```
Fish Shell:
echo $argv

Komentorivi syötteellä: 
fish args.fish hello world

Tuloste: 
hello world
```

Voit myös käyttää “$argc” muuttujaa, joka sisältää kokonaisluvun annettujen argumenttien määrästä.

```
Fish Shell:
echo $argc

Komentorivi syötteellä:
fish args.fish hello world

Tuloste:
2
```

Nyt tiedät, miten lukea ja käyttää komentorivin argumentteja Fish Shell -ympäristössä!

## Syvempää tarkastelua

Fish Shellilla on myös muita tapoja käsitellä komentorivin argumentteja, kuten esimerkiksi “string match” -toiminto. Tämä mahdollistaa tietyn argumentin etsimisen ja käsittelemisen. Voit myös hyödyntää “for loop” -toimintoa, joka käy läpi kaikki annetut argumentit ja suorittaa halutun toiminnon jokaiselle argumentille.

Fish Shell tarjoaa myös erilaisia muuttujia ja funktioita komentorivin argumenttien käsittelyyn. Kannattaa tutustua Fish Shellin dokumentaatioon saadaksesi lisätietoja ja oppiaksesi lisää hyödyllisiä toimintoja!

## Katso myös

- [Fish Shell dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Official Fish Shell Github repository](https://github.com/fish-shell/fish-shell)
- [Fish Shell Wiki](https://github.com/fish-shell/fish-shell/wiki)