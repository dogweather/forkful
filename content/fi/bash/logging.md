---
title:                "Lokitus"
date:                  2024-01-26T00:59:29.574352-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lokitus"

category:             "Bash"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/logging.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lokitus on käytäntö, jossa ohjelman suoritusprosessien tapahtumat, virheet ja muu merkittävä tieto tallennetaan tiedostoon tai tulostevirtaan. Ohjelmoijat tekevät tätä seuratakseen sovellustensa käyttäytymistä, debugatakseen ongelmia ja ylläpitääkseen historiallista tietueta operaatioista, jotka voivat auttaa tulevaisuuden vianetsinnässä.

## Miten:

Bashissa lokituksesta voi tehdä niinkin yksinkertaista kuin uudelleenohjaamalla tai lisäämällä ulostuloa tiedostoon. Tässä yksinkertainen esimerkki:

```Bash
echo "Aloitetaan skripti..." >> script.log
# Skriptin komentosi tässä
echo "Skripti suoritettu $(date)" >> script.log
```

Johonkin edistyneempään voit sisällyttää syslogin systeemilaajuiseen lokiin:

```Bash
logger "Oma viesti skriptistäni"
```

`logger` lähettää lokiviestin syslog-palvelulle, joka sitten käsittelee sen järjestelmän syslog-konfiguraation mukaisesti.

Esimerkki ulostulo tallennettuna `script.log`-tiedostoon:

```Bash
Aloitetaan skripti...
Skripti suoritettu ti maalis 23 09:26:35 PDT 2021
```

## Syväluotaus

Perinteisesti Unix-tyyppisissä järjestelmissä lokitus on mahdollistettu syslog-palvelun avulla, jolloin erilaiset sovellukset ja järjestelmän osat voivat lokioida viestejä keskitetysti. Tämä mahdollistaa standardoitujen lokitusmekanismien käyttöönoton koko järjestelmässä.

Vaihtoehtoja harkittaessa jotkut saattavat tutkia `syslog-ng`- tai `rsyslog`-palveluita edistyneempiä lokitusominaisuuksia varten tai kirjoittaa lokeja aikasarjatietokantaan analytiikkaa varten. Sovelluksille, joilla on korkeampi kompleksisuuden taso, voi jopa skriptauskielelle kuten Bashille olla järkevää käyttää omistettua lokituskirjastoa tai -sovellusta kuten Log4j (Java-ekosysteemissä) tai Monolog (PHP:ssä), jotka voivat tarjota rakenteellisia ja konfiguroitavia lokitusvaihtoehtoja.

Toteuttamasi lokitusmenetelmä riippuu suuresti sovelluksesi vaatimuksista. Jos tarvitset yksinkertaista ulostuloa skriptin edistymisen seuraamiseen, viestien lisääminen tiedostoon on helppoa ja kätevää. Kuitenkin monipuolisempaa ja kestävämpää lokitusta varten haluat integroitua lokitusjärjestelmään, joka tukee ominaisuuksia kuten lokien kiertoa, lokitasoja ja etälokitoimintaa.

## Katso Myös

- `man`-sivut `logger`- ja `syslog`-funktioille ovat aina ystäväsi, kokeile `man logger` tai `man syslog`.
- Syvällisemmän näkemyksen järjestelmälokista saamiseksi harkitse `rsyslog`- tai `syslog-ng`-dokumentaation lukemista.
- Jos haluat tietää lisää historiallisesta kontekstista ja periaatteista lokituksen takana Unix-tyyppisissä järjestelmissä, `Syslog`-protokolla, joka on dokumentoitu RFC 5424:ssä, tarjoaa kattavaa tietoa.
