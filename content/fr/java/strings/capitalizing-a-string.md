---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:43.567344-07:00
description: "Comment faire : La biblioth\xE8que standard de Java ne propose pas de\
  \ m\xE9thode directe pour capitaliser des cha\xEEnes enti\xE8res en une seule fois,\
  \ mais vous\u2026"
lastmod: '2024-03-13T22:44:57.616570-06:00'
model: gpt-4-0125-preview
summary: "La biblioth\xE8que standard de Java ne propose pas de m\xE9thode directe\
  \ pour capitaliser des cha\xEEnes enti\xE8res en une seule fois, mais vous pouvez\
  \ accomplir cela avec une combinaison de m\xE9thodes int\xE9gr\xE9es."
title: "Mettre en majuscule une cha\xEEne"
weight: 2
---

## Comment faire :
La bibliothèque standard de Java ne propose pas de méthode directe pour capitaliser des chaînes entières en une seule fois, mais vous pouvez accomplir cela avec une combinaison de méthodes intégrées. Pour des besoins plus sophistiqués, des bibliothèques tierces comme Apache Commons Lang offrent des solutions simples.

### En utilisant les méthodes intégrées de Java
Pour capitaliser une chaîne sans bibliothèques externes, vous pouvez diviser la chaîne en mots, capitaliser la première lettre de chacun, puis les réunir. Voici une approche simple :

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // Sortie : "Hello, World!"
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

Ce fragment de code convertit toute la chaîne en minuscules, puis itère à travers chaque caractère, en capitalisant la première lettre de chaque mot. Il considère les espaces, les points et les apostrophes comme des séparateurs de mots.

### En utilisant Apache Commons Lang
La bibliothèque Apache Commons Lang fournit une solution plus élégante avec la méthode `WordUtils.capitalizeFully()`, qui gère divers cas limites et délimiteurs pour vous :

```java
// Ajouter la dépendance : org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // Sortie : "Hello, World!"
    }
}
```

Pour utiliser cette méthode, vous devrez ajouter la bibliothèque Apache Commons Lang à votre projet. Cette méthode de la bibliothèque ne se contente pas de capitaliser la première lettre de chaque mot mais convertit également le reste des lettres de chaque mot en minuscules, garantissant un schéma de capitalisation cohérent dans toute la chaîne.
