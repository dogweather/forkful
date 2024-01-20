---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyyntöjen lähettäminen on tapa, jolla ohjelmat kommunikoivat verkon yli, hakevat tietoja tai lähettävät tietoa palvelimelle. Ohjelmoijat tekevät tämän, koska se on tärkeä osa modernia, verkottunutta ohjelmointia.

## Näin teet:

C#-kielellä me voimme lähettää HTTP-pyyntöjä `HttpClient` luokan avulla. Alla on yksinkertainen esimerkki GET-pyynnöstä:

```C#
HttpClient client = new HttpClient();
string url = "http://example.com";
HttpResponseMessage response = await client.GetAsync(url);
string result = await response.Content.ReadAsStringAsync();
Console.WriteLine(result);
```
Tämä ohjelma lähettää GET-pyynnön `http://example.com` -sivustolle, lukee vastauksen ja tulostaa sen konsoliin.

## Syvempi Katsaus

HTTP-pyynnöt ovat olleet olemassa vuodesta 1991 lähtien, jolloin HTTP/1.0 spesifikaatio julkaistiin. C# kehittäjänä sinulla on useita vaihtoehtoja HTTP-pyyntöjen lähettämiseen, kuten `HttpClient`, `WebRequest`, `RestSharp`, jne. `HttpClient` on kuitenkin nykyinen suositus, koska se on tehokas ja joustava työkalu monenlaisiin tilanteisiin.

HTTP-pyynnön lähettämiseen liittyvät yksityiskohdat saattavat hieman vaihdella eri kirjastojen kesken, mutta yleensä ne noudattavat samaa perusprosessia: luodaan yhteys palvelimeen, lähetetään pyyntö ja tulkitaan vastaus.

## Katso Myös

1. [`HttpClient` luokan dokumentaatio](https://docs.microsoft.com/fi-fi/dotnet/api/system.net.http.httpclient?view=net-5.0)
2. [HTTP-pyyntöjen lähetys C#-kielellä](https://docs.microsoft.com/fi-fi/aspnet/web-api/overview/advanced/calling-a-web-api-from-a-net-client)
3. [HTTP:n historia](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/Evolution_of_HTTP)