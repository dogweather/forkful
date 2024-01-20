---
title:                "Mettre une chaîne de caractères en majuscules"
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Mettre en majuscules une chaîne de caractères signifie transformer toutes ses lettres en caractères majuscules. Les programmeurs le font souvent pour uniformiser les données textuelles ou pour des besoins esthétiques, comme dans les titres.

## Comment faire :

```java
public class CapitalizeExample {
    public static void main(String[] args) {
        String original = "bonjour le monde!";
        String capitalized = original.toUpperCase();
        System.out.println(capitalized);
    }
}
```

Sortie :
```
BONJOUR LE MONDE!
```

## Plongée Profonde

Historiquement, la conversion de textes en majuscules remonte aux premières machines à écrire et aux premiers ordinateurs qui ne distinguaient pas toujours les majuscules des minuscules. Dans Java, `toUpperCase()` est la méthode utilisée pour capitaliser des chaînes. Elle est sensible à la locale : par défaut, elle utilise la locale de l'environnement d'exécution, mais on peut spécifier une locale si nécessaire. Une alternative est d'utiliser la bibliothèque Apache Commons Lang avec `StringUtils.upperCase(String str)`, qui peut être pratique pour ses nombreuses méthodes de manipulation de chaînes. En interne, `toUpperCase()` va itérer sur les caractères de la chaîne, les convertir un par un en majuscules en utilisant leur code Unicode et reconstruire une nouvelle chaîne.

## Voir Aussi

- La documentation officielle de la méthode `String.toUpperCase()` : [Oracle Docs](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#toUpperCase())
- Un guide sur la classe `String` de Java : [Java String Guide](https://www.baeldung.com/java-string)
- Apache Commons Lang `StringUtils` : [Apache Commons Lang](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)