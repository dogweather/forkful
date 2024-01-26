---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:48:37.351233-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Generoidaan satunnaislukuja monista syistä – testidata, simulaatiot, ja turvallisuus. Bashissa se on kätevä tapa saada nopeasti arvoja, joita ei voi ennakoida.

## How to: (Kuinka tehdä:)
```Bash
# Satunnaisluku 0-99 väliltä
echo $((RANDOM % 100))

# Satunnaisluku 1-100 väliltä
echo $((RANDOM % 100 + 1))

# Kerroksiaan esimerkki
for i in {1..10}; do
  echo $((RANDOM % 100 + 1))
done
```
Esimerkkitulostus yhdelle suoritukselle voi olla:
```
57
23
...
41
```

## Deep Dive (Syväsukellus)
Bashin sisäänrakennettu `$RANDOM` on todellakin kyseleen pseudo-satunnaislukugeneraattori (PSL). Se on ollut osa bashia jo pitkästä aikaa, käyttäen lineaarikuoren säätymää (LCG) algoritmia. Vaikka `$RANDOM` on kätevä, se ei ole kryptografisesti turvallinen – jos turvallisuus on tärkeää, käytä jotain kuten `openssl` tai `/dev/random`. Tärkein tässä on muistaa modulo-operaation (%) käyttäminen arvojen skaalaamiseksi tarvitsemasi alueen sisään.

## See Also (Katso Myös)
- Advanced Bash-Scripting Guide - Satunnaislukujen tuottamisesta: https://www.tldp.org/LDP/abs/html/randomvar.html
- OpenSSL:n käyttö satunnaislukujen tuottamiseen: https://www.openssl.org/
