---
date: 2024-01-20 17:59:57.707271-07:00
description: "How to: (Kuinka tehd\xE4:) Fish Shellissa HTTP-pyynn\xF6n l\xE4hett\xE4\
  minen onnistuu `curl` tai `httpie` komentojen avulla."
lastmod: '2024-04-05T21:53:58.569915-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Fish Shellissa HTTP-pyynn\xF6n l\xE4hett\xE4minen onnistuu\
  \ `curl` tai `httpie` komentojen avulla."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

## How to: (Kuinka tehdä:)
Fish Shellissa HTTP-pyynnön lähettäminen onnistuu `curl` tai `httpie` komentojen avulla. 

```Fish Shell
# Lähetä GET-pyyntö
curl http://httpbin.org/get 

# POST-pyyntö datan kanssa
curl -d 'data=HelloWorld' http://httpbin.org/post

# Käyttäen httpie:tä, jos se on asennettu
http GET http://httpbin.org/get
http POST http://httpbin.org/post data=HelloWorld
```

Esimerkkituloste `curl`:lla GET-pyynnön jälkeen:

```Fish Shell
{
  "args": {}, 
  "headers": {
    "Accept": "*/*", 
    "Host": "httpbin.org", 
    "User-Agent": "curl/7.64.1"
  }, 
  "origin": "93.123.45.67, 93.123.45.67", 
  "url": "https://httpbin.org/get"
}
```

## Deep Dive (Sukellus syvyyksiin)
HTTP-pyynnöt ovat perustana webin toiminnalle. Ne alkoivat 90-luvulla yksinkertaisina GET- ja POST-metodeina ja ovat kasvaneet laajaksi metodi- ja statuskoodien kokoelmaksi. Fish Shellissä `curl` ja `httpie` ovat suosittuja työkaluja HTTP-pyyntöjen lähettämiseen, tarjoten näppärän komentorivirajapinnan.

`curl` on laajalti käytössä oleva komentorivityökalu, joka tukee monia protokollia. Se on sisäänrakennettu useimpiin Unix-pohjaisiin järjestelmiin. `httpie`, toisaalta, on modernimpi vaihtoehto, jolla on helpommin luettava ulostulo.

Fishissä, kuten muissakin shellissä, näiden työkalujen avulla voi automatisoida verkkopyyntöjä osana skriptejä, tai yhdistää niitä muihin komentoihin datan käsittelyä varten.

## See Also (Katso myös)
- [curl projektin kotisivu](https://curl.haxx.se/)
- [httpie kotisivu](https://httpie.io/)
- [Fish Shellin dokumentaatio](https://fishshell.com/docs/current/index.html)
- [HTTP-protokollan yleiskatsaus](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
