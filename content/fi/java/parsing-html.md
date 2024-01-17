---
title:                "Html-analysointi"
html_title:           "Java: Html-analysointi"
simple_title:         "Html-analysointi"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
HTML:n jäsentäminen (parseaminen) tarkoittaa HTML-koodin muuttamista selkeämmäksi ja helpommin käsiteltäväksi muodoksi. Tämä on tärkeää, koska monet ohjelmat käyttävät HTML-koodia ja haluavat saada siitä tietoa. Näiden tietojen avulla ohjelmat voivat tehdä asioita kuten näyttää verkkosivuja ja hakea tietoja.

## Miten:
Koodiesimerkki Java-kielellä ja sen tulosteet:

```Java
// Luodaan HTML-lähtömerkkijono
String html = "<p>Tervetuloa <strong>Java-ohjelmointimaailmaan!</strong></p>";

// Jäsentäminen
Document doc = Jsoup.parse(html);

// Hae elementti id:n avulla
Element element = doc.getElementById("firstElement");

// Tulosta elementin sisältö
System.out.println(element.text());

// Tulostus:
// Tervetuloa Java-ohjelmointimaailmaan!
```

Tämä koodi luo HTML-lähtömerkkijonon, jäsentää sen ja hakee tietyn elementin sisällön id:n avulla. Lopuksi tulostetaan elementin sisältö, joka tässä tapauksessa on teksti "Tervetuloa Java-ohjelmointimaailmaan!".

## Syvemmälle:
HTML parseamisella on pitkä historia, ja sitä käytetään laajasti web-ohjelmoinnissa. Java-kielessä on useita kirjastoja, kuten Jsoup, joka sallii helpomman tavan jäsentää HTML-koodia. On myös muita tapoja, kuten regular expressionien käyttäminen, mutta se ei ole yhtä luotettava ja voi olla hankalampi toteuttaa.

## Lue myös:
- [Jsoup kirjaston dokumentaatio](https://jsoup.org/)
- [Vinkkejä HTML:n jäsentämiseen Java-kielellä](https://dzone.com/articles/modern-way-of-parsing-html-in-java)