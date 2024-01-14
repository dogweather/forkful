---
title:                "Java: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une fonctionnalité clé en programmation Java qui permet de combiner plusieurs chaînes en une seule. Cela peut être utile pour créer des messages personnalisés, des rapports ou pour formater des données de manière cohérente.

## Comment Faire

La concaténation de chaînes en Java peut être réalisée avec l'opérateur "+" ou la méthode "concat()". Voici un exemple de code utilisant l'opérateur "+" :

```Java
String firstName = "Jean";
String lastName = "Dupont";

String fullName = firstName + " " + lastName;

System.out.println(fullName);
```

Cela produira l'output suivant :

```
Jean Dupont
```

Vous pouvez également utiliser la méthode "concat()" pour concaténer des chaînes :

```Java
String firstName = "Jean";
String lastName = "Dupont";

String fullName = firstName.concat(" ").concat(lastName);

System.out.println(fullName);
```

Le résultat sera le même que précédemment :

```
Jean Dupont
```

## Plongée en Profondeur

Lors de la concaténation de chaînes de caractères, il est important de comprendre que les objets String en Java sont immuables, c'est-à-dire qu'ils ne peuvent pas être modifiés directement. Ainsi, chaque fois que vous concaténez une chaîne, un nouvel objet String est créé en mémoire.

Il est donc plus efficace en termes de performances d'utiliser la méthode "concat()" plutôt que l'opérateur "+" lorsque vous devez concaténer plusieurs chaînes.

De plus, la concaténation de chaînes peut être plus complexe lorsque les chaînes impliquées ont des encodages différents ou des caractères spéciaux. Dans ces cas, il peut être nécessaire de convertir les chaînes en tableaux de caractères et de les manipuler avant de les concaténer.

## Voir Aussi

- [Documentation officielle Java sur la classe String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Guide complet sur la concaténation de chaînes de caractères en Java](https://stackabuse.com/concatenating-strings-in-java/)