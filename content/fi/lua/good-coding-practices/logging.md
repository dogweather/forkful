---
title:                "Lokitus"
aliases:
- /fi/lua/logging.md
date:                  2024-01-26T01:07:36.097219-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lokitus"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/logging.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?

Lokitus on käytäntö tallentaa tapahtumia, virheitä ja muita merkittäviä datapisteitä, jotka ilmenevät ohjelmistosovelluksen elinkaaren aikana. Ohjelmoijat käyttävät lokitietoja avuksi vianetsinnässä, järjestelmän terveyden valvonnassa, käyttäjäkäyttäytymisen analysoinnissa sekä turvallisuuden ja lainsäädännön noudattamisen auditoimiseksi.

## Kuinka:

Lua ei sisällä sisäänrakennettua lokitusjärjestelmää, mutta yksinkertaisen lokitusfunktion toteuttaminen on suoraviivaista. Alla on perusesimerkki tällaisesta funktiosta:

```lua
function logMessage(level, message)
    -- Peruslokitus konsoliin
    print(string.format("[%s] %s: %s", os.date("%Y-%m-%d %H:%M:%S"), level, message))
end

-- Käyttöesimerkkejä:
logMessage("INFO", "Sovellus on käynnistynyt.")
logMessage("WARN", "Vanhentunut funktion kutsu havaittu.")
logMessage("ERROR", "Tiedoston avaaminen epäonnistui.")
```

Kun yllä olevaa koodia ajetaan, näet tulosteen kuten seuraavasti:
```
[2023-03-22 14:55:01] INFO: Sovellus on käynnistynyt.
[2023-03-22 14:55:01] WARN: Vanhentunut funktion kutsu havaittu.
[2023-03-22 14:55:01] ERROR: Tiedoston avaaminen epäonnistui.
```

Monimutkaisempiin lokitusvaatimuksiin voidaan käyttää kolmannen osapuolen kirjastoja, kuten LuaLogging, joka tuo lisäominaisuuksia kuten lokitasot, useat käsittelijät ja muotoiluspesifikaatiot.

## Syväsukellus

Historiallisesti lokitus on ollut olennainen osa ohjelmistojen vianmääritystä ja se on vakiintunut käytäntö programmeroinnin alkuaikojen jälkeen. Lokituksen merkitystä ei voida liioitella, sillä se toimii 'mustana laatikkona' järjestelmän vikatilanteessa tarjoten näkemyksiä ongelmien juurisyihin.

Vaikka yllä oleva esimerkki täyttää vain perustarpeet, olemassa on paljon vaihtoehtoja, joissa on rikkaampi ominaisuusjoukko. Näitä ovat muun muassa:

- Lokitiedostojen kirjoittaminen pysyvään tallennukseen.
- Lokitiedostojen kierrättäminen levyn tilankäytön hallitsemiseksi.
- Lokien lähettäminen lokien hallintajärjestelmään tai palveluun.

Sukeltaessa lokitusjärjestelmän toteuttamiseen päätöskohtiin voi kuulua sopivien lokitasojen valitseminen (debug, info, warn, error, fatal jne.), lokiviestien rakenteen suunnittelu (esim. JSON helppoon jäsentämiseen) ja varmistaminen, että lokitusaktiviteetti ei merkittävästi vaikuta suorituskykyyn.

Hajautetuissa järjestelmissä on tavallista käyttää keskitettyjä lokien hallintaratkaisuja, kuten ELK (Elasticsearch, Logstash ja Kibana) tai Splunk, jotka voivat aggregoida lokit monista lähteistä, tarjota tehokkaita hakutoimintoja ja visualisoida dataa helpottaakseen vianmääritystä ja analyysiä.

## Katso Myös

- LuaLogging-kirjasto GitHubissa: https://github.com/lunarmodules/lualogging
- Johdatus ELK Pinolle: https://www.elastic.co/what-is/elk-stack
- Luau-käyttäjien wiki lokituksesta: http://lua-users.org/wiki/LoggingCategory
- Keskustelu lokituksen suorituskykyvaikutuksista Luassa: http://www.freelists.org/post/luajit/Logging-what-does-it-cost,1
