---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTTP-pyyntö perustodennuksella on prosessi, jossa lähettää serverille suojattuja tietoja. Ohjelmoijat käyttävät sitä varmistaakseen, että vain valtuutetut osapuolet pääsevät käsiksi tietoihin.

## Näin se tehdään:

Lua-koodin avulla perustodennuksen HTTP-pyyntö suoritetaan seuraavasti:

```Lua
http = require("socket.http")
ltn12 = require("ltn12")

function http_auth(url, user, pass)
    local responsebody = {}

    http.request{
        url = url,
        sink = ltn12.sink.table(responsebody),
        user = user,
        password = pass
    } 

    return table.concat(responsebody)
end
```

Ja esimerkki sen käytöstä:

```Lua
res = http_auth('http://example.com', 'username', 'password')
print(res)
```

Tässä esimerkissä pyynnön vastauksen on tulostettu konsolille.

## Syvempi sukellus

HTTP-pyyntö perustodennuksella on ollut osa ohjelmointia siitä asti, kun suojattu verkkosisältö tuli mahdolliseksi. Valitettavasti se ei ole kaikkein turvallisin menetelmä todennukseen, joten se on parasta jättää sisältöön, joka ei ole erityisen arkaluonteista.

Vaihtoehtoisia menetelmiä ovat token-pohjainen todennus ja OAuth, jotka tarjoavat ylimääräistä turvallisuutta.

Lua noudattaa tiettyä syntaksia perustodennuksen suhteen. 'user' ja 'password' kenttien tulee olla osana http.request-kutsua. Serverin saa vastauksena yhdistetyn viestin.

## Katso myös

- [Lua:n virallinen dokumentaatio](http://www.lua.org/manual/5.4/)
- [HTTP-autentikointi: perus- ja ruoansulatuspääsyautentikointi](https://developer.mozilla.org/fi/docs/Web/HTTP/Authentication)
- [OAuth 2.0 -protokolla](https://oauth.net/2/)