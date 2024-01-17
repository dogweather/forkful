---
title:                "Perusautentikoinnin lähettäminen http-pyynnöllä"
html_title:           "Javascript: Perusautentikoinnin lähettäminen http-pyynnöllä"
simple_title:         "Perusautentikoinnin lähettäminen http-pyynnöllä"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Kun lähetät HTTP-pyynnön perustunnuksella, lähetät tietokoneella olevan koodin ja pyydät tietoa jostakin verkkosivusta tai palvelusta. Ohjelmoijat tekevät tätä usein hankkimaan tai välittämään tietoja palvelimen ja käyttäjän välillä turvallisesti ja luotettavasti.

## Kuinka:

```javascript
let url = "https://www.example.com/";
let username = "käyttäjänimi";
let password = "salasana";
// Luo HTTP-pyyntö käyttäen perustunnusta
let request = new XMLHttpRequest();
request.open('GET', url);
request.setRequestHeader('Authorization', 'Basic ' + btoa(username + ":" + password));
request.send();
```

Esimerkiksi käyttäjänimen ja salasanan avulla voimme lähettää pyynnön verkkosivulle ja vastaanottaa tarvitsemamme tiedot.

## Syvyyteen:

HTTP-pyynnöt perustunnuksella ovat yksi tapa varmistaa tietoturva ja luotettavuus verkkoyhteyden kautta. Tämä menetelmä kehitettiin alkuaikoina lähinnä sähköpostin ja FTP-palvelimien käytön yhteydessä. Nykyään on olemassa myös muita menetelmiä, kuten OAuth, joita voidaan käyttää autentikointiin ja autorisointiin. Perustunnus ei turvaa tietoja yhtä tehokkaasti kuin muut menetelmät, mutta se on edelleen turvallinen tapa siirtää tietoja verkon yli.

## Katso myös:

Jotta opit lisää HTTP-pyynnöistä ja perustunnuksella lähetettävistä pyynnöistä, tutustu seuraaviin lähteisiin:

- [MDN Web Docs - HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [W3Schools - HTTP Authorization](https://www.w3schools.com/tags/att_http_authorization.asp)
- [Google Developers - OAuth Overview](https://developers.google.com/identity/protocols/oauth2#httprest)