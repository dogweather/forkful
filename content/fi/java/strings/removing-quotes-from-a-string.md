---
date: 2024-01-26 03:39:47.703538-07:00
description: "Miten: Kiskotaanpa nuo \xE4rsytt\xE4v\xE4t lainausmerkit pois tekstist\xE4\
  mme. K\xE4yt\xE4mme `replace()`-metodia nopeisiin korjauksiin ja regexi\xE4 koviin\
  \ p\xE4hkin\xF6ihin."
lastmod: '2024-03-13T22:44:56.432735-06:00'
model: gpt-4-0125-preview
summary: "Kiskotaanpa nuo \xE4rsytt\xE4v\xE4t lainausmerkit pois tekstist\xE4mme."
title: Merkkijonosta lainausmerkkien poistaminen
weight: 9
---

## Miten:
Kiskotaanpa nuo ärsyttävät lainausmerkit pois tekstistämme. Käytämme `replace()`-metodia nopeisiin korjauksiin ja regexiä koviin pähkinöihin.

```java
public class QuoteRemover {
    public static void main(String[] args) {
        String stringWithQuotes = "\"Hello, 'World'!\"";
        String withoutQuotes = stringWithQuotes.replace("\"", "").replace("'", "");
        System.out.println(withoutQuotes); // Hello, World!

        // Nyt regexillä kaavion ystäville
        String stringWithMixedQuotes = "\"Java\" ja 'Ohjelmointi'";
        String cleanString = stringWithMixedQuotes.replaceAll("[\"']", "");
        System.out.println(cleanString); // Java ja Ohjelmointi
    }
}
```

## Syväsukellus
Aikoinaan lainausmerkit merkkijonoissa eivät olleet kovin suuri vaiva—järjestelmät olivat yksinkertaisempia, eikä data ollut yhtä sotkuista. Monimutkaisten datamuotojen (JSON, XML) ja datanvaihdon tarpeen myötä lainausmerkkien hallinta muuttui avainasiaksi. Vaihtoehdoista puhuttaessa, toki, voisit kirjoittaa parserin, käydä läpi jokaisen merkin ja rakentaa uuden merkkijonon (voisi olla hauskaa sateisena päivänä). On myös kolmannen osapuolen kirjastoja, jotka voivat käsitellä tätä hienovaraisemmin, tarjoten vaihtoehtoja merkkien välttämiseen poistamisen sijaan, tai käsitellä erilaisia lainausmerkkejä paikallisten sääntöjen mukaan. Toteutusta ajatellen, pidä mielessä, että lainausmerkkien poistaminen ilman kontekstia voi muuttaa datan merkitystä tai rakennetta—aina tulee harkita "miksi" ennen "miten".

## Katso myös
- Syvemmälle sukeltamiseksi regexiin, tutustu virallisiin Java-dokumentteihin: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html
- Tarvitsetko paeta lainausmerkkejä sen sijaan, että poistaisit niitä? Stack Overflow on apunasi: https://stackoverflow.com/questions/383551/escape-string-for-sql-insert
- JSON-käsittely Javassa? Todennäköisesti kohtaat lainausmerkkejä usein. Tässä lähtökohta: https://www.oracle.com/technical-resources/articles/java/json.html
