---
title:                "Elixir: Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi
 Monet Elixir-ohjelmoijat joutuvat jossain vaiheessa törmäämään päivämäärän ja merkkijonon konvertointiin. Tämän lukemattoman kerran tehdyn tehtävän motivaatio voi vaihdella - työnantajan vaatimusten täyttämiseen, kansainvälisen standartin noudattamiseen tai yleisesti ottaen päivämäärän esittämiseen muodossa, joka on helppo lukea ja ymmärtää. 

## Miten
Voit muuntaa päivämäärän merkkijonoksi erilaisilla tavoilla Elixirissä. Yksinkertaisin tapa on käyttää tiivistelmäfunktiota `to_string`. Tässä on esimerkki:
```Elixir
DateTime.utc_now() |> to_string()
```
<img src="https://i.imgur.com/1rXtITF.png" alt="output" width="250"/>

Voit myös muuttaa muotoa ja määrittää tarkemman päivämäärän esitysmuodon lisäämällä toisen parametrin `to_string` -funktioon. Esimerkiksi `to_string(DateTime.utc_now(), "{ISO}")` antaa tuloksena ISO 8601 -muotoisen päivämäärän. Katso tarkemmat ohjeet Elixirin dokumentaatiosta.

Mikäli haluat tarkemman hallinnan päivämäärän konvertoinnissa, voit myös käyttää `naive_datetime` -funktiota, joka ottaa parametrina päivämäärän tiedot, kuten vuoden, kuukauden ja päivän. Tämän jälkeen voit käyttää haluamiasi muotoilumerkkejä `strftime`-funktiolla luodaksesi haluamasi päivämäärän esitysmuodon.

Kiinnitettäköön huomiota siihen, että päivämäärän muuntaminen merkkijonoksi ja takaisin voi vaikuttaa alkuperäiseen päivämäärään, etenkin UTC-ajan ja aikavyöhykkeiden osalta. On hyvä olla tietoinen tästä mahdollisesta ongelmasta ja käyttää sitä sopivanlaisissa tilanteissa.

## Syvempi sukellus
Elixirin `to_string` -funktio on täysin riippuvainen `DateTime` -moduulista, joka tarjoaa useita metodeja päivämäärän hallintaan ja muuntamiseen. Kannattaa tutustua tähän moduuliin tarkemmin, jotta voit löytää itsellesi sopivimman ratkaisun päivämäärän esittämiseen.

Toinen vaihtoehto päivämäärän muuttamiseen merkkijonoksi on käyttää `Calendar` -moduulia, joka tarjoaa monipuolisemman tavan käsitellä päivämääriä. Tämä moduuli sisältää myös `Calendar.ISO` -muotoilumerkin, joka on hyödyllinen päivämäärän muotoilussa.

## Katso myös
- [Elixirin dokumentaatio DateTime-moduulille](https://hexdocs.pm/elixir/DateTime.html)
- [Elixirin dokumentaatio Calendar-moduulille](https://hexdocs.pm/elixir/Calendar.html)
- [ISO 8601 standardi](https://en.wikipedia.org/wiki/ISO_8601)