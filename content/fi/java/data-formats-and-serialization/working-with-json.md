---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:25.288240-07:00
description: "Ty\xF6skentely JSON:n (JavaScript Object Notation) parissa tarkoittaa\
  \ t\xE4m\xE4n kevyen datanvaihtoformaatista hy\xF6dynt\xE4mist\xE4 Java-sovelluksissasi.\
  \ Ohjelmoijat\u2026"
lastmod: '2024-03-13T22:44:56.467028-06:00'
model: gpt-4-0125-preview
summary: "Ty\xF6skentely JSON:n (JavaScript Object Notation) parissa tarkoittaa t\xE4\
  m\xE4n kevyen datanvaihtoformaatista hy\xF6dynt\xE4mist\xE4 Java-sovelluksissasi."
title: "Ty\xF6skentely JSON:n kanssa"
weight: 38
---

## Mikä & Miksi?
Työskentely JSON:n (JavaScript Object Notation) parissa tarkoittaa tämän kevyen datanvaihtoformaatista hyödyntämistä Java-sovelluksissasi. Ohjelmoijat suosivat JSONia rakenneellisen datan sarjoittamiseen ja siirtämiseen verkon yli sekä datan helppoon konfigurointiin ja tallentamiseen, koska se on ihmisen luettavissa ja kielestä riippumaton.

## Kuinka:
Kääritään hihat ja ryhdytään koodaamaan JSONin kanssa Javassa.

Ensimmäiseksi, tarvitset JSON-käsittelykirjaston kuten `Jackson` tai `Google Gson`. Tässä käytämme `Jacksonia`, joten lisää tämä riippuvuus `pom.xml`-tiedostoosi:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Nyt, sarjoitetaan (kirjoitetaan) yksinkertainen Java-olio JSONiksi:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonEsimerkki {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Henkilo henkilo = new Henkilo("Alex", 30);
            String json = mapper.writeValueAsString(henkilo);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Henkilo {
    public String nimi;
    public int ika;

    public Henkilo(String nimi, int ika) {
        this.nimi = nimi;
        this.ika = ika;
    }
}
```

Tulosteen pitäisi olla:

```json
{"nimi":"Alex","ika":30}
```

Nyt, deserialisoidaan (luetaan) JSON takaisin Java-olioksi:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonEsimerkki {
    public static void main(String[] args) {
        String json = "{\"nimi\":\"Alex\",\"ika\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Henkilo henkilo = mapper.readValue(json, Henkilo.class);
            System.out.println(henkilo.nimi + " on " + henkilo.ika + " vuotta vanha.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Tulosteena on:

```
Alex on 30 vuotta vanha.
```

## Syväluotaus
JSONin yksinkertaisuus ja tehokkuus ovat tehneet siitä de facto -standardin datan vaihtoon webissä, syrjäyttäen XML:n valtaistuimeltaan. Esitelty 2000-luvun alussa, JSON on johdettu JavaScriptistä, mutta nyt se on tuettu useimmissa kielissä.

Vaihtoehtoja JSONille ovat mm. XML, joka on verbosimpi, ja binaarimuodot kuten Protocol Buffers tai MessagePack, jotka eivät ole ihmisen luettavia mutta ovat tehokkaampia koossa ja nopeudessa. Kullakin on käyttötapauksensa; valinta riippuu erityisistä datatarpeistasi ja kontekstistasi.

Javassa, `Jackson`in ja `Gson`in lisäksi, meillä on `JsonB` ja `org.json` muita kirjastoja JSONin käsittelyyn. Jackson tarjoaa stream-pohjaisen prosessoinnin ja on tunnettu nopeudestaan, kun taas Gsonia juhlitaan sen helppokäyttöisyydestä. JsonB on osa Jakarta EE:tä, tarjoten standardoidumman lähestymistavan.

Kun toteutat JSONia, muista käsitellä poikkeuksesi asianmukaisesti - koodisi tulisi olla vankka huonoja syötteitä vastaan. Harkitse myös automaattisen datan sitomisen turvallisuusimpikaatioita – varmista aina syötteesi!

## Katso Myös
- [Jackson-projekti](https://github.com/FasterXML/jackson)
- [Gson-projekti](https://github.com/google/gson)
- [JSON-spesifikaatio](https://www.json.org/json-en.html)
- [JsonB-spesifikaatio](https://jakarta.ee/specifications/jsonb/)
