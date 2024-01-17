---
title:                "Perusautentikoinnin lähettäminen http-pyynnöllä"
html_title:           "Java: Perusautentikoinnin lähettäminen http-pyynnöllä"
simple_title:         "Perusautentikoinnin lähettäminen http-pyynnöllä"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Mitä tehdään ja miksi?

HTTP-pyyntöjen lähettämisellä perusautentikoinnin kanssa tarkoitetaan yksinkertaisesti käyttäjän tunnistamista ja luvan antamista palvelimelle. Tämä on yleinen käytäntö verkkosovelluksissa, ja sen avulla voimme suojata tiettyjä resursseja ja estää asiattomien käyttäjien pääsyn niihin.

# Näin teet sen:

```Java
// Luodaan HttpClient -olio
HttpClient client = HttpClient.newHttpClient();
// Luodaan pyyntöosoite
URI uri = URI.create("https://www.example.com/api/resource");
// Luodaan pyyntö
HttpRequest request = HttpRequest.newBuilder(uri).build();
// Lisätään tarvittavat headerit perusautentikointia varten
String authString = username + ":" + password;
String encodedAuth = Base64.getEncoder().encodeToString(authString.getBytes());
request = request.newBuilder().header("Authorization", "Basic " + encodedAuth).build();
// Lähetetään pyyntö ja otetaan vastaan paluupyyntö
HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
// Tulostetaan vastauksen statuskoodi
System.out.println(response.statusCode());
// Tulostetaan vastauksen sisältö
System.out.println(response.body());
```

Esimerkkitulos:

```
200
{"name": "John Smith", "age": 25, "role": "admin"}
```

# Syväsukellus:

Perusautentikointi on yksi tapa suojata verkkoresursseja, ja se on ollut käytössä jo pitkään. Nykyisin on kuitenkin olemassa myös muita tapoja, kuten OAuth ja käyttäjäavaimet.

HTTP-pyyntöihin liittyvät headerit ovat tärkeitä, sillä ne sisältävät tiedot pyynnön lähettäjästä ja halutuista parametreista. Perusautentikoinnin tapauksessa headeriin lisätään base64-koodattu versio käyttäjän tunnuksesta ja salasanasta.

# Katso myös:

- Dokumentaatio Java HttpClientistä: https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html
- Artikkeli perusautentikoinnista: https://www.example.com/article/basic-authentication 
- Tietoa eri autentikointitavoista: https://www.example.com/authentication-types