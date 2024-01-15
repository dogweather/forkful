---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Clojure: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Todennäköisesti olet kuullut käsitteen HTTP request, mutta miksi sinun kannattaisi oikeastaan välittää siitä? No, jos haluat lähettää tai vastaanottaa tietoa internetissä, niin silloin sinun täytyy käyttää HTTP requesteja. Ne ovat tärkeä osa verkkosovellusten toimintaa ja tarjoavat mahdollisuuden kommunikoida eri palvelimien kanssa.

## Näin teet sen

Voit lähettää HTTP requestin käyttämällä Clojuren `clj-http` kirjastoa. Asenna se ensin projektisi riippuvuuksiin ja sitten voit tehdä seuraavanlaisia HTTP pyyntöjä:

```Clojure
(require [clj-http.client :as client])

; GET pyyntö
(client/get "https://www.example.com")

; POST pyyntö parametreilla ja otsikoilla
(client/post "https://www.example.com"
  :params {:key "value"}
  :headers {"Content-Type" "application/json"})
```

Tämä lähettää GET pyynnön `www.example.com` osoitteeseen ja POST pyynnön `www.example.com` osoitteeseen käyttäen JSON muotoa ja antaa sille `{key: value}` parametrin.

## Syvemmälle

Voit myös määrittää lisäparametreja lähettämillesi pyynnöille, kuten aikakatkaisun ja virheiden käsittelyn. Voit myös lähettää datan bodyssa, esimerkiksi tiedoston tai Clojure kartan.

Voit myös käyttää `clj-http`a testaamaan API:n endpointtien toimintaa. Esimerkiksi voit lähettää POST pyynnön käyttäen vääriä parametreja ja katsoa miten API vastaa.

## Katso myös

- [Virallinen clj-http dokumentaatio](https://github.com/dakrone/clj-http)
- [Clojuren virallinen kotisivu](https://clojure.org/)
- [HTTP requestit ja vastaukset yksinkertaisesti](https://medium.com/@kennethjhan/http-requests-and-responses-in-plain-english-c7901625ac32)