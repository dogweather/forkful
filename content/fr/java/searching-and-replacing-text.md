---
title:                "Java: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches essentielles lors de la programmation en Java. Cela peut vous aider à automatiser des modifications massives dans votre code source ou à remplacer des parties spécifiques d'une chaîne de caractères. Cela peut également vous faire gagner du temps et améliorer la qualité globale de votre code.

## Comment faire

Voici un exemple de code Java qui montre comment effectuer une recherche et un remplacement de texte à l'aide de la méthode `replace()` :

```Java
String texte = "Bonjour à tous !";
String nouveauTexte = texte.replace("Bonjour", "Salut");
System.out.println(nouveauTexte);
```

Cela produira la sortie suivante :

```
Salut à tous !
```

Dans cet exemple, la méthode `replace()` remplace toutes les occurrences du mot "Bonjour" par "Salut" dans la chaîne de caractères `texte`.

## Plongée en profondeur

Il est important de noter que la méthode `replace()` ne modifie pas directement la chaîne de caractères d'origine. Au lieu de cela, elle créé une nouvelle chaîne de caractères avec les modifications apportées. Cela signifie que si vous voulez conserver ces changements, vous devez attribuer le résultat de la méthode `replace()` à une nouvelle variable. Sinon, le texte d'origine restera inchangé.

De plus, la méthode `replace()` est sensible à la casse. Cela signifie qu'elle fera la distinction entre les majuscules et les minuscules lors de la recherche et du remplacement de texte. Si vous voulez ignorer la casse, vous pouvez utiliser la méthode `replaceAll()`.

## Voir aussi

- [Documentation officielle de la méthode replace() de Java](https://docs.oracle.com/javase/9/docs/api/java/lang/String.html#replace-char-char-)
- [Tutoriel YouTube sur la recherche et le remplacement de texte en Java](https://www.youtube.com/watch?v=ys2zeEsMY1s)