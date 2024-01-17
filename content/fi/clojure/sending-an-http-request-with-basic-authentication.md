---
title:                "Lähettämässä http-pyyntöä perusautentikoinnilla"
html_title:           "Clojure: Lähettämässä http-pyyntöä perusautentikoinnilla"
simple_title:         "Lähettämässä http-pyyntöä perusautentikoinnilla"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?
Jos olet ohjelmoija ja haluat lähettää HTTP-pyynnön perusautentikoinnilla, niin tämä artikkeli on sinulle! Perusautentikointi on yksinkertainen tapa lähettää pyyntöön mukana käyttäjänimi ja salasana, jolla voidaan valvoa pääsyä palvelimelle tai resursseihin.

# Miten valitaan?
Clojure:lla voit lähettää HTTP-pyynnön perusautentikoinnilla käyttämällä "with-basic-auth" funktiota ja antamalla sille käyttäjänimen ja salasanan parametreina. Tässä on esimerkki:

```Clojure
(with-basic-auth "käyttäjänimi" "salasana"
  (http/get "http://esimerkki.com/api/käyttäjät"))
```

Tuloksena saat kutsun käyttäjät-resurssiin autentikoinnilla, jossa käytetään "käyttäjänimi" ja "salasana".

# Syvällisempi sukellus
Perusautentikointi on osa HTTP-protokollaa ja otettiin käyttöön jo vuonna 1999. Ennen tätä käytettiin paljon epävarmempia menetelmiä käyttäjän tunnistamiseen. On myös muita tapoja autentikoida HTTP-pyyntöjä, kuten OAuth ja API-avaimet.

# Katso myös
Voit lukea lisää HTTP-pyyntöjen lähettämisestä Clojure:lla täältä: https://clojure.github.io/http-client/
Ja lisätietoja perusautentikoinnista ja sen historiasta löydät täältä: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication