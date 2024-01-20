---
title:                "JSON-tiedostojen käsittely"
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
JSON on dataformaatti tiedon vaihtoon. Käyttävät JSONia koska se on helppo lukea ja kirjoittaa; yhteensopiva useimpien ohjelmointikielten kanssa.

## How to: (Kuinka tehdä:)
Java käsittelee JSONia `org.json` kirjaston tai `Jackson` ja `Gson` kautta. Tässä esimerkki `org.json`:lla.

```java
import org.json.JSONObject;

public class JsonExample {
    public static void main(String[] args) {
        // Luodaan JSON-objekti
        JSONObject obj = new JSONObject();
        obj.put("nimi", "Matti Meikäläinen");
        obj.put("ikä", 30);
        obj.put("onkoOhjelmoija", true);

        // Tulostetaan JSON-merkkijono
        System.out.println(obj.toString());
    }
}
```

Esimerkin tulostus:
```
{"nimi":"Matti Meikäläinen","ikä":30,"onkoOhjelmoija":true}
```

## Deep Dive (Syväsukellus):
JSON (JavaScript Object Notation) kehitettiin 2000-luvun alussa ja yleistyi nopeasti AJAX-sovellusten kanssa. XML oli ennen JSONia, mutta JSON voitti suosiotaan keveytensä ja selkeytensä ansiosta. `org.json` on vanhin Java-kirjasto JSONille. `Jackson` ja `Gson` ovat nopeampia ja tarjoavat lisää toiminnallisuutta, kuten datan sidonta Java-olioiden ja JSONin välillä.

## See Also (Katso Myös):
- [Gson-kirjaston GitHub-sivu](https://github.com/google/gson)
- [Jackson-kirjaston GitHub-sivu](https://github.com/FasterXML/jackson)
- [Java JSON API (JSON-P)](https://javaee.github.io/jsonp/)