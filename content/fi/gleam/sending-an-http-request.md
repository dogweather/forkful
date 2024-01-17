---
title:                "Lähettää http-pyyntö"
html_title:           "Gleam: Lähettää http-pyyntö"
simple_title:         "Lähettää http-pyyntö"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyynnön lähettäminen on tapa tietokoneohjelman kysyä ja vastaanottaa dataa verkkopalvelimelta. Tätä tehdään usein ohjelmoidessa verkkosovelluksia, jotka tarvitsevat tiedonkulun ulkopuoliseen palvelimeen.

## Miten:

Esimerkiksi, jos haluat lähettää HTTP-pyynnön osoitteeseen "www.sivusto.fi" ja saada vastauksena "200 OK", niin koodisi näyttää tältä:

```Gleam
request = http.Client.get("www.sivusto.fi")
``` 
```Gleam
expect(request.status_code) == 200
```

Tässä koodissa käytetään HTTP-kirjastoa "http", joka tarjoaa metodeja pyyntöjen lähettämiseen ja vastauksien käsittelyyn. Koodi kutsuu "Client" -funktiota ja antaa parametrina osoitteen, johon halutaan lähettää pyyntö. Tämän jälkeen vertaillaan vastauksen "status_code" -arvoa odotetun arvon kanssa.

## Syvemmälle:

HTTP-protokolla kehitettiin alunperin vuonna 1989 Tim Berners-Leen toimesta. Sittemmin on kehitetty muita protokollia, kuten HTTPS, joka salakirjoittaa tiedonkulun ja tekee siitä turvallisemman. On myös olemassa muita käytössä olevia kirjastoja HTTP-pyyntöjen lähettämiseen, kuten "fetch" ja "requests", mutta Gleamin "http" -kirjasto on helppo ja tehokas vaihtoehto.

## Katso myös:

- https://gleam.run/libraries/http
- https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol