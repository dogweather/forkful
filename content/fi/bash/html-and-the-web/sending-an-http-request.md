---
date: 2024-01-20 17:59:15.006325-07:00
description: "How to: (Kuinka tehd\xE4:) Bashissa voit k\xE4ytt\xE4\xE4 `curl` tai\
  \ `wget` komentoa HTTP-pyynt\xF6jen tekemiseen. Yksinkertaisin esimerkki on GET-pyynt\xF6\
  ."
lastmod: '2024-04-05T21:53:58.310580-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Bashissa voit k\xE4ytt\xE4\xE4 `curl` tai `wget` komentoa\
  \ HTTP-pyynt\xF6jen tekemiseen."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

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
