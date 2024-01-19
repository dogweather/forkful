---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Stringin Interpolointi Java-ohjelmoinnissa: Miten ja Miksi?

## Mikä & Miksi?
Stringin interpolointi tarkoittaa muuttujien ja ilmaisujen upottamista tekstin sisään. Ohjelmoijat tekevät sitä, koska se mahdollistaa dynaamisen sisällön luomisen ja lisää koodin luettavuutta.

## Kuinka:
Tässä on esimerkki stringin interpoloinnista Javassa käyttäen `String.format` -metodia:

```Java
String nimi = "Matti";
float keskiarvo = 85.5f;
String viesti = String.format("Hei %s, keskiarvosi on %.1f", nimi, keskiarvo);
System.out.println(viesti);
```

Tuotos:
```Java
Hei Matti, keskiarvosi on 85.5
```
Interpoloidun stringin luominen on noin yksinkertaista!

## Syvempi kulku:
Stringin interpolointi on ollut käytössä vuosikymmeniä ja sitä tukevat useat ohjelmointikielet, myös Javan varhaisemmat versiot. Vaikka Java ei tarjoa sisäänrakennettua syntaksia, kuten esim. Python tai JavaScript, `String.format` -metodi on voimakas ja joustava työkalu tässä kontekstissa.

Vaihtoehtoisena tapana, voit luoda interpoloidun stringin käyttäen `+` -operaattoria sekä useita muita Java-kirjastoja, kuten Apache Commons Lang3's `StringSubstitutor`. Jokainen menetelmä eroaa suorituskyvyn, joustavuuden ja syntaksin osalta.

Täysin ymmärtääkseen ja hyödyntämään String.format -metodia Java-ohjelmistokehityksessä, on tärkeää tutustua Javan `Formatter` -luokkaan ja sen syntaksiin.

## Katso myös:
Javan virallinen dokumentaatio `Formatter` -luokasta: https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/util/Formatter.html

Apache Commons Lang3 `StringSubstitutor` -luokka: https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/text/StringSubstitutor.html