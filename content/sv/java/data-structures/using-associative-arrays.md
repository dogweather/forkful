---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:38.984965-07:00
description: "Hur man g\xF6r: Java har inte inbyggda associativa arrayer som vissa\
  \ spr\xE5k g\xF6r, men det tillhandah\xE5ller `Map`-interfacet och klasser som `HashMap`\
  \ och\u2026"
lastmod: '2024-03-13T22:44:37.780508-06:00'
model: gpt-4-0125-preview
summary: "Java har inte inbyggda associativa arrayer som vissa spr\xE5k g\xF6r, men\
  \ det tillhandah\xE5ller `Map`-interfacet och klasser som `HashMap` och `TreeMap`\
  \ f\xF6r att fylla den rollen."
title: "Att anv\xE4nda associativa arrayer"
weight: 15
---

## Hur man gör:
Java har inte inbyggda associativa arrayer som vissa språk gör, men det tillhandahåller `Map`-interfacet och klasser som `HashMap` och `TreeMap` för att fylla den rollen. Så här använder du en `HashMap`:

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // Skapar en HashMap
        Map<String, Integer> ageOfFriends = new HashMap<>();
        
        // Lägger till element
        ageOfFriends.put("Alice", 24);
        ageOfFriends.put("Bob", 30);
        ageOfFriends.put("Charlie", 28);

        // Får tillgång till element
        System.out.println("Alices ålder: " + ageOfFriends.get("Alice"));
        
        // Hantering av icke-existerande nycklar
        System.out.println("Åldern för någon som inte finns i mappen: " + ageOfFriends.getOrDefault("Dan", -1));

        // Itererar över elementen
        for (Map.Entry<String, Integer> entry : ageOfFriends.entrySet()) {
            System.out.println(entry.getKey() + " är " + entry.getValue() + " år gammal.");
        }
    }
}
```

Exempelutdata:

```
Alices ålder: 24
Åldern för någon som inte finns i mappen: -1
Alice är 24 år gammal.
Bob är 30 år gammal.
Charlie är 28 år gammal.
```

`HashMap` är bara en implementering. Om dina nycklar är unika och du behöver dem sorterade, överväg `TreeMap`. För en karta som behåller insättningsordningen är `LinkedHashMap` din vän.

## Fördjupning
Map i Java är en del av Collections Framework, introducerad i JDK 1.2, men har sett betydande förbättringar under åren, inklusive introduktionen av `forEach`-metoden i Java 8 för enklare iteration över poster. Valet av map-implementering (`HashMap`, `LinkedHashMap`, `TreeMap`) bör styras av dina specifika behov när det gäller ordning och prestanda. Till exempel erbjuder `HashMap` O(1) tidsprestanda för de grundläggande operationerna (get och put), förutsatt att hashfunktionen sprider elementen lämpligt bland hinkarna. Men om du behöver sortering baserat på naturlig ordning eller anpassade jämförare, är `TreeMap` att gå till, vilket ger O(log n) tid för insättning och uppslagning.

Innan `Map` introducerades implementerades associativa arrayer vanligtvis med två parallella arrayer (en för nycklar, en för värden) eller anpassade datastrukturer med mindre effektivitet. Nuvarande alternativ till `Map` och dess implementeringar kan inkludera tredjepartsbibliotek som erbjuder specialiserade kartor, såsom bidirektionella kartor (BiMap i Googles Guava-bibliotek) för fall där du effektivt behöver hitta en nyckel genom dess värde. Dock, för de flesta användningsfall i Java, är standardbibliotekets kartor robusta och flexibla nog för att hantera uppgiften.
