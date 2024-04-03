---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:51.149927-07:00
description: "Merkkijonon alkukirjaimen suurentaminen tarkoittaa jokaisen sanan ensimm\xE4\
  isen kirjaimen muuttamista suuraakkoseksi samalla varmistaen, ett\xE4 loput\u2026"
lastmod: '2024-03-13T22:44:56.428173-06:00'
model: gpt-4-0125-preview
summary: "Merkkijonon alkukirjaimen suurentaminen tarkoittaa jokaisen sanan ensimm\xE4\
  isen kirjaimen muuttamista suuraakkoseksi samalla varmistaen, ett\xE4 loput kirjaimet\
  \ pysyv\xE4t pienaakkosina."
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
weight: 2
---

## Mikä ja miksi?
Merkkijonon alkukirjaimen suurentaminen tarkoittaa jokaisen sanan ensimmäisen kirjaimen muuttamista suuraakkoseksi samalla varmistaen, että loput kirjaimet pysyvät pienaakkosina. Tämä yleinen merkkijonon käsittelytehtävä on hyödyllinen tekstien muotoiluun sovelluksissa, kuten käyttäjänimien tai otsikoiden valmistelussa näyttöä varten sopimuksen tai kieliopillisen oikeellisuuden mukaisesti.

## Kuinka:
Javan vakio kirjasto ei tarjoa suoraa metodia koko merkkijonojen alkukirjaimen suurentamiseksi kerralla, mutta tämän voi saavuttaa yhdistämällä sisäänrakennettuja metodeja. Monimutkaisempiin tarpeisiin kolmannen osapuolen kirjastot, kuten Apache Commons Lang, tarjoavat suoraviivaisia ratkaisuja.

### Javan sisäänrakennettujen metodien käyttäminen
Merkkijonon alkukirjaimen suurentaminen ulkoisia kirjastoja käyttämättä voidaan tehdä jakamalla merkkijono sanoiksi, suurentamalla kunkin sanan ensimmäinen kirjain ja sitten liittämällä ne uudelleen yhteen. Tässä on yksinkertainen lähestymistapa:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // Tulostaa: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

Tämä koodinpätkä muuntaa koko merkkijonon pienaakkosiksi, sitten se käy läpi jokaisen merkin, suurentaen kunkin sanan ensimmäisen kirjaimen. Se pitää välejä, pisteitä ja heittomerkkejä sananerottimina.

### Apache Commons Langin käyttäminen

Apache Commons Lang -kirjasto tarjoaa tyylikkäämmän ratkaisun `WordUtils.capitalizeFully()` metodilla, joka käsittelee puolestasi erilaiset reunatapaukset ja erotinmerkit:

```java
// Lisää riippuvuus: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // Tulostaa: "Hello, World!"
    }
}
```

Tämän metodin käyttämiseksi sinun on lisättävä Apache Commons Lang -kirjasto projektiisi. Tämä kirjastometodi ei ainoastaan suurenna jokaisen sanan ensimmäistä kirjainta, vaan muuntaa myös jokaisen sanan muut kirjaimet pienaakkosiksi, varmistaen johdonmukaisen alkukirjaimen suuruuden läpi koko merkkijonon.
