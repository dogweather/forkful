---
title:                "Verkkosivun lataaminen"
html_title:           "Java: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Mitä & Miksi?
Verkkosivun lataaminen tarkoittaa web-sivun koodin kopioimista internetsivulta tietokoneellesi. Tämä on hyödyllistä ohjelmoijille, sillä se sallii heidän käyttää verkkosivun dataa ja sisältöä sovellustensa kehittämiseen.

Kuinka tehdä:
Java-koodi lohkoissa esitetyt koodiesimerkit ja tulostustulos.

```Java
// Käytetään URL-olioita ja BufferedReader-luokkaa
URL osoite = new URL("https://www.esimerkkisivu.com");
BufferedReader lukija = new BufferedReader(new InputStreamReader(osoite.openStream()));

// Luetaan tiedosto rivi riviltä
String rivi;
while ((rivi = lukija.readLine()) != null) {
    System.out.println(rivi);
}

// Suljetaan lukija
lukija.close();
```

Tulostus:

```
<html>
<head>
<title>Esimerkkisivu</title>
</head>
<body>
<p>Tämä on esimerkkisivu!</p>
</body>
</html>
```

Deep Dive:
Verkkosivujen lataaminen on tärkeä osa web-kehitystä ja ohjelmointia. Sitä voidaan käyttää monissa eri tilanteissa, kuten datan hakemiseen, web-robottien luomiseen tai tiedon muokkaamiseen.

Vaihtoehtoisia tapoja verkkosivujen lataamiseen ovat esimerkiksi JavaScript-ohjelmointikielen käyttö, joka sallii dynaamisten sivujen lataamisen, sekä CURL-komento, joka mahdollistaa tiedon lähettämisen ja vastaanottamisen käyttäen erilaisia protokollia.

Tarkemmin katsottuna verkkosivun lataaminen käyttää HTTP-protokollaa kommunikoidessaan verkkosivun kanssa, ja se käyttää myös Java IO-pakettia tiedoston lukemiseen ja käsittelyyn.

See Also:
Lisätietoa verkkosivujen lataamisesta Java-kielellä löytyy Java SE:n verkkosivuilta. Lisäksi täältä löytyy jQuery-kirjastoon perustuva esimerkki lataamisesta.