---
date: 2024-01-20 17:45:44.938009-07:00
description: "Extraire des sous-cha\xEEnes, c'est d\xE9couper des petits morceaux\
  \ d'une cha\xEEne de caract\xE8res. Les programmeurs le font pour isoler des donn\xE9\
  es pr\xE9cises,\u2026"
lastmod: '2024-03-13T22:44:57.625483-06:00'
model: gpt-4-1106-preview
summary: "Extraire des sous-cha\xEEnes, c'est d\xE9couper des petits morceaux d'une\
  \ cha\xEEne de caract\xE8res."
title: "Extraction de sous-cha\xEEnes"
weight: 6
---

## How to: (Comment faire :)
```java
public class SubstringExample {
    public static void main(String[] args) {
        String originalString = "Bonjour, bienvenue en programmation Java!";
        String extractedString = originalString.substring(9, 19);
        
        System.out.println(extractedString); // Affiche 'bienvenue'
        
        // Extraire une sous-chaîne jusqu'à la fin
        String endString = originalString.substring(20);
        System.out.println(endString); // Affiche 'en programmation Java!'
    }
}
```
Output:
```
bienvenue
en programmation Java!
```

## Deep Dive (Plongée en profondeur)
Historiquement, l’extraction de sous-chaînes est fondamentale en programmation depuis que le texte est devenu une partie intégrante des interfaces utilisateur. `substring` est depuis longtemps présente en Java ; son utilisation est raffinée au fil de versions pour plus de simplicité et de performance. Les alternatives incluent `split`, pour fractionner une chaîne autour d'un motif régulier, et les API de manipulation de texte comme `StringBuilder` ou `StringBuffer`. Pour l’extraction, Java crée une nouvelle chaîne, ce qui peut avoir un impact sur la mémoire si pas géré correctement avec de très longues chaînes ou dans des boucles serrées.

## See Also (Voir aussi)
- [Class String (Java Platform SE API)](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,int))
- [Regular Expressions in Java (Pattern/Matcher)](https://docs.oracle.com/javase/tutorial/essential/regex/)
