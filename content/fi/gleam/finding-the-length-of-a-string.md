---
title:                "Gleam: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi selvittää merkkijonon pituuden? Yksinkertainen vastaus on, että monissa ohjelmoinnissa se on olennainen tieto, jota tarvitaan erilaisten laskelmien ja oikeiden tulosten saavuttamiseksi. Voit myös haluta tietää merkkijonon pituuden esimerkiksi validoidaksesi käyttäjän syötteen. Mutta riippumatta syystä, Gleam-ohjelmointikielen avulla voit helposti löytää ja käsitellä merkkijonon pituutta.

## Miten

Gleam-ohjelmointikieli tarjoaa yksinkertaisen ja tehokkaan tavan löytää merkkijonon pituus. Voit käyttää sisäänrakennettua `.len()` funktiota, joka palauttaa merkkijonon pituuden luvun muodossa. Alla on esimerkki Gleam-koodista, joka käyttää `.len()` funktiota merkkijonon pituuden määrittämiseksi:

```Gleam 
// Alusta merkkijono
string := "Hei maailma!"

// Käytä .len() funktiota
pituus := .len(string)

// Tulosta tulos
io.println("Merkkijonon pituus on " ++ length)
```

Tämän koodin tulisi tulostaa "Merkkijonon pituus on 12". Kuten näet, `.len()` funktiota käytetään merkkijonon nimen edessä aivan kuin kutsuisit sitä metodia.

## Syventyvä sukellus

Vaikka `.len()` funktio on suunniteltu palauttamaan merkkijonon pituuden, se toimii myös muiden tyyppien kanssa. Voit käyttää sitä esimerkiksi taulukon tai listan pituuden määrittämiseen. `.len()` funktio palauttaa aina luvun ja se on tyypiltään `Int` eli kokonaisluku.

On myös hyvä huomata, että Gleam käsittelee merkkijonoja Unicode-muodossa, mikä tarkoittaa, että se tukee monenkielisiä merkkejä ja erikoismerkkejä. Tämä voi vaikuttaa merkkijonon pituuteen, koska esimerkiksi kiinalaiset merkit vievät enemmän tilaa kuin latinalaiset kirjaimet. `.len()` funktio mittaa merkkijonon pituuden merkkien määrässä, eikä niiden fyysisessä näkyvyydessä.

## Katso myös

- Gleam ohjelmointikielen virallinen dokumentaatio: https://gleam.run/getting-started
- Gleam ohjelmointikielen virallinen GitHub-sivusto: https://github.com/gleam-lang
- Gleam ohjelmointikielen Slack-yhteisö: https://gleam-community.slack.com/
- Verkko-ohjelmointi kurssi (in Finnish): https://www.avoinyliopisto.fi/fi/web/seamk-verkossa/home?p_p_id=t_1&p_p_lifecycle=0&p_p_state=maximized&p_p_mode=view&_t_1_struts_action=%2Fext%2Fwww_auc_attachments%2FdownloadFile&_t_1_urlTitle=web_verkko_opintoja&pes=_5696