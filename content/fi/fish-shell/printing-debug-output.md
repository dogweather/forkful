---
title:    "Fish Shell: Tulostaminen virheenjäljitystiedostoon"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi?: Miksi tulostaa debug-tekstilähtö?
Tulostamalla debug-tekstilähtöä voit tarkkailla ohjelmasi toimintaa ja selvittää mahdollisia virheitä tai ongelmakohtia helpommin.

## Kuinka aloittaa:
Debug-tekstilähdön tulostaminen on helppoa Fish Shell -ohjelmointikielessä. Yksinkertaisesti käytä ```echo``` -komentoa tulostaaksesi haluamasi tekstin. Voit myös käyttää ```printf``` -komentoa, joka antaa enemmän muotoiluvaihtoehtoja.

```Fish Shell
echo "Tämä on debug-tekstilähtö."
```

Tämä tulostaa "Tämä on debug-tekstilähtö" terminaaliin.

```Fish Shell
printf "Virhe ilmestyi numerossa %d." 50
```

Tämä tulostaa "Virhe ilmestyi numerossa 50." terminaliin.

Voit myös käyttää ```set -x``` komentoa, joka tulostaa automaattisesti jokaisen Fish Shell komennon ja sen sisältämät argumentit.

## Syvä sukellus:
Debug-tekstilähdön tulostaminen voi auttaa sinua selvittämään oletusten oikeellisuutta sekä löytämään virheitä tai ongelmakohtia koodistasi. Voit tulostaa esimerkiksi tiettyjen muuttujien arvoja tarkistaaksesi, ovatko ne haluamasi arvoisia tai seuraatko haluttua logiikkaa.

Voit myös käyttää erilaisia ​​tulostusmuotoja, kuten värejä tai lihavointia, jotta debug-tekstilähdöstä tulee helposti erottuva ja luettava.

## Katso myös:
- [Fish Shell dokumentaatio](https://fishshell.com/docs/current/)
- [Debugging in Fish Shell](https://medium.com/@brunostripoli/debugging-in-fish-shell-4cde63d18866)
- [Fish Shell: A Comprehensive Guide](https://wezfurlong.org/wezterm/2019/01/using-fishshell.html)