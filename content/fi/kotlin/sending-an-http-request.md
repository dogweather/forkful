---
title:                "Http-pyynnön lähettäminen"
html_title:           "Kotlin: Http-pyynnön lähettäminen"
simple_title:         "Http-pyynnön lähettäminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Lähettäessäsi HTTP-pyynnön, tarkoitat käytännössä sitä, että saat yhteyden verkossa olevaan palvelimeen ja pyydät sitä tekemään jotakin haluamaasi toimintoa. Ohjelmoijat usein käyttävät tätä tekniikkaa, kun he haluavat hakea tietoa verkkosivuilta tai lähettää dataa palvelimelle.

## Näin teet sen:
```
Kotlin fun main() { 
    val url = "https://example.com" // aseta url -muuttuja, johon tallennetaan verkkosivun osoite
    val client = HttpClient() // alustetaan http -asiakas
    val response: HttpResponse  = client.get(url) // lähetetään get -pyyntö palvelimelle ja tallennetaan vastaus muuttujaan
    println(response.content) // tulostetaan vastauksen sisältö
}
```

## Syvemmälle:
Lähetystä HTTP-pyyntöjä on käytetty jo useiden vuosien ajan ja se on yksi yleisimmistä tekniikoista kommunikoida palvelimien kanssa. On myös olemassa muita vaihtoehtoja, kuten REST, joka on kevyempi tapa kommunikoida palvelimien kanssa. HTTP-pyynnön lähettämisen toteutus sisältää erilaisia protokollia ja metodeja, mutta yleisesti ottaen se vaatii vain yksinkertaisen määritellyn osoitteen ja HTTP-metodin valinnan.

## Katso myös:
- [Kotlin Documentaatio: HTTP-pyynnön lähettäminen](https://kotlinlang.org/docs/reference/http-client.html)
- [Java HTTP-pyyntöjen lähettäminen](https://www.baeldung.com/java-http-request)