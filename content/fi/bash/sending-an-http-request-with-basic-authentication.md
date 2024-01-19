---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTTP-pyynnön lähettäminen perusautentikoinnilla on tekniikka, jolla lähetetään dataa tai pyydetään tietoja verkossa. Ohjelmoijat tekevät sen, kun heidän on mentävä pääsynhallintalistojen tai salasanojen läpi palvelimella.

## Kuinka:

```Bash
# Esimerkki perusautentikoinnilla varustetun HTTP-pyynnön lähettämisestä Bashilla.
# Käytämme CURL-komentoa.

USERNAME='käyttäjätunnus'
PASSWORD='salasana'
URL='https://esimerkki.com'

# Lähetämme GET-pyynnön salasanalla.
curl -u ${USERNAME}:${PASSWORD} ${URL}
```

Kirjoitustulos näyttää tältä:

```Bash
{
  "vastauksen arvo": "esimerkki"
}
```

## Sukellus syvemmälle:

Perusautentikointi on vanhin ja yksinkertaisin HTTP-autentikointimekanismi, joka on luotu 1990-luvun alkupuolella. Tästä huolimatta se on edelleen hyödyllinen, kun tarvitaan yksinkertaista tunnistautumismenettelyä.

Alternatiiveiksi voi tutkia OAuth, token-pohjainen autentikointi tai jopa SSL-sertifikaatin autentikointi. Valinta riippuu kuitenkin järjestelmän yksittäisistä tarpeista ja monimutkaisuudesta.

Perusautentikointi toimii lähettämällä HTTP-otsakkeina base64-koodattu käyttäjänimi ja salasana. Tämä tarkoittaa, ettei tietoa ole suojattu, joten HTTPS-protokollan käyttö on suositeltavaa tällaisten pyyntöjen suojaamiseksi.

## Katso myös:

1. [HTTP-autentikointi: Perus- ja Digest -autentikointi](https://developer.mozilla.org/fi/docs/Web/HTTP/Authentication)
2. [Curl-komennon virallinen dokumentaatio](https://curl.se/docs/manpage.html)
3. [Turvallisuus ja HTTP-pyynnöt](https://owasp.org/www-community/Secure_HTTP)