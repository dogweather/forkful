---
aliases:
- /fi/bash/sending-an-http-request/
date: 2024-01-20 17:59:15.006325-07:00
description: "HTTP-pyynt\xF6 on verkkoresurssin (yleens\xE4 web-sivun tai API:n) hakemiskutsu.\
  \ Ohjelmoijat l\xE4hett\xE4v\xE4t n\xE4it\xE4 pyynt\xF6j\xE4 tietojen noutamiseksi,\
  \ l\xE4hett\xE4miseksi,\u2026"
lastmod: 2024-02-18 23:09:07.801190
model: gpt-4-1106-preview
summary: "HTTP-pyynt\xF6 on verkkoresurssin (yleens\xE4 web-sivun tai API:n) hakemiskutsu.\
  \ Ohjelmoijat l\xE4hett\xE4v\xE4t n\xE4it\xE4 pyynt\xF6j\xE4 tietojen noutamiseksi,\
  \ l\xE4hett\xE4miseksi,\u2026"
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
HTTP-pyyntö on verkkoresurssin (yleensä web-sivun tai API:n) hakemiskutsu. Ohjelmoijat lähettävät näitä pyyntöjä tietojen noutamiseksi, lähettämiseksi, päivittämiseksi tai poistamiseksi.

## How to: (Kuinka tehdä:)
Bashissa voit käyttää `curl` tai `wget` komentoa HTTP-pyyntöjen tekemiseen. Yksinkertaisin esimerkki on GET-pyyntö.

```Bash
# GET-pyyntö curl-komennolla
curl https://example.com

# Tulostaa verkkosivun HTML-koodin
```

```Bash
# POST-pyyntö curl-komennolla tiedon lähettämiseen
curl -d "param1=value1&param2=value2" -X POST https://example.com/api

# Vastaanottaa ja näyttää serverin vastauksen
```

## Deep Dive (Sukellus syvyyksiin):
Aikaisemmin tiedon lähettäminen palvelimelle vaati kokonaisia skriptejä tai ohjelmistoja. Bashin `curl` ja `wget` ovat tervetulleita työkaluja, sillä niitä on helppo käyttää komentoriviltä.

`curl` on monipuolinen ja tukee lähes kaikkia internetin siirtoprotokollia. `wget` on hieman vanhempi, mutta se on suosittu tiedostojen lataamisessa.

Käyttöliittymädetaljit, kuten headerit ja keksit, ovat säädettävissä. Tämä mahdollistaa esimerkiksi simuloituja istuntoja tai käyttäjän tunnistamisen.

```Bash
# Lähetä JSON-dataa ja aseta Content-Type header curl-komennolla
curl -H "Content-Type: application/json" -d '{"key1":"value1", "key2":"value2"}' -X POST https://example.com/api
```

## See Also (Katso Myös):
- `curl` manuaalisivu: [curl.haxx.se](https://curl.haxx.se/docs/manpage.html)
- `wget` manuaalisivu: [GNU Wget Manual](https://www.gnu.org/software/wget/manual/wget.html)
- Bash-skriptauksen perusteet: [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- HTTP-protokollan ymmärtäminen: [MDN Web Docs - HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
