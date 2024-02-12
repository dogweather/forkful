---
title:                "Willekeurige getallen genereren"
aliases:
- /nl/java/generating-random-numbers/
date:                  2024-01-28T22:01:28.172432-07:00
model:                 gpt-4-0125-preview
simple_title:         "Willekeurige getallen genereren"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/java/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Random (willekeurige) getallen genereren gaat over het produceren van onvoorspelbare sequenties of enkelvoudige waarden binnen een gedefinieerd bereik. Programmeurs gebruiken deze techniek om verschillende redenen, waaronder simulaties, spellen, beveiligingstoepassingen en bemonsteringsmethoden om algoritmen onder verschillende voorwaarden te testen.

## Hoe:

In Java kan het genereren van willekeurige getallen worden bereikt met de `Random` klasse uit het `java.util` pakket, of de `ThreadLocalRandom` en `SecureRandom` klassen voor specifieke gebruikssituaties. De volgende voorbeelden illustreren hoe deze klassen te gebruiken.

### Gebruik van de `Random` klasse
De `Random` klasse biedt een manier om eenvoudige pseudo-willekeurige getallen te genereren.

```Java
import java.util.Random;

public class RandomVoorbeeld {
    public static void main(String[] args) {
        Random rand = new Random(); // Maak een Random object

        int randInt = rand.nextInt(50); // Genereert een willekeurig geheel getal van 0 tot 49
        double randDubbel = rand.nextDouble(); // Genereert een willekeurig dubbel tussen 0,0 en 1,0
        boolean randBooleaans = rand.nextBoolean(); // Genereert een willekeurige boolean
        
        System.out.println("Willekeurige Int: " + randInt);
        System.out.println("Willekeurige Dubbel: " + randDubbel);
        System.out.println("Willekeurige Boolean: " + randBooleaans);
    }
}
```

### Gebruik van de `ThreadLocalRandom` klasse
Voor gelijktijdige applicaties is `ThreadLocalRandom` efficiënter dan `Random`.

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomVoorbeeld {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // Van 1 tot 100
        double randDubbel = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // Van 1,0 tot 10,0
        
        System.out.println("Willekeurige Int: " + randInt);
        System.out.println("Willekeurige Dubbel: " + randDubbel);
    }
}
```

### Gebruik van de `SecureRandom` klasse
Voor cryptografische operaties biedt `SecureRandom` een hoger niveau van veiligheid.

```Java
import java.security.SecureRandom;

public class SecureRandomVoorbeeld {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // Vult bytes met beveiligde willekeurige getallen
        
        System.out.println("Beveiligde Willekeurige Bytes:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## Verdieping

De generatie van willekeurige getallen is aanzienlijk geëvolueerd sinds de beginjaren van de informatica. Java's `Random` klasse gebruikt een lineaire congruente formule om pseudo-willekeurige getallen te genereren, die deterministisch zijn en niet geschikt voor toepassingen met hoge veiligheidseisen. Dit leidde tot de introductie van `SecureRandom`, welke meer geavanceerde algoritmes gebruikt (bijv. SHA1PRNG) om cryptografisch sterke willekeurige getallen te produceren.

Echter, `Random` en `SecureRandom` hebben hun tekortkomingen, zoals prestatievermindering in multithreaded omgevingen. De `ThreadLocalRandom` klasse is geïntroduceerd in Java 7 om dit probleem aan te pakken door thread-lokale willekeurige getallengeneratoren te voorzien, wat prestaties in gelijktijdige applicaties aanzienlijk verbetert.

Hoewel deze klassen de meeste behoeften dekken, zouden ontwikkelaars voor extreem grote schaal of gespecialiseerde vereisten extra bibliotheken kunnen verkennen of aangepaste oplossingen ontwikkelen. Het is essentieel om de juiste benadering te kiezen op basis van de veiligheidsbehoeften en prestatievereisten van de use case.
