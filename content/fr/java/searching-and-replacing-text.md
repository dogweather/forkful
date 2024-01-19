---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

La recherche et le remplacement de texte sont des tâches courantes en programmation, utilisées pour modifier des chaînes de caractères. Les programmeurs les utilisent pour manipuler les données, corriger les erreurs ou optimiser le code.

## Comment faire:

Voici comment rechercher et remplacer du texte en Java à l'aide de la méthode `replace()`:

```Java
String s1="Bonjour World!";
s1=s1.replace("World","Monde");
System.out.println(s1); // Sortie: "Bonjour Monde!"
```
Dans cet exemple, nous remplaçons "World" par "Monde". La sortie sera "Bonjour Monde!"

## Plongée Profonde:

Historiquement, les opérations de recherche et de remplacement sont nées des éditeurs de texte et des traitements de texte. En Java, la classe `String` fournit diverses méthodes pour effectuer ces opérations, `replace()` étant l'une des plus simples.

Il existe plusieurs alternatives à `replace()`, telles que `replaceAll()` et `replaceFirst()`. La méthode `replaceAll()` remplace toutes les occurrences de la sous-chaîne, tandis que `replaceFirst()` n'en remplace que la première.

En ce qui concerne les détails de mise en œuvre, sachez que `replace()` est une méthode non destructive. Cela signifie qu'elle ne modifie pas l'objet String original mais renvoie un nouvel objet String.

## Voir Aussi:

Pour approfondir vos connaissances sur la recherche et le remplacement de texte en Java, consultez les sources liées suivantes:

- [Oracle Java Documentation](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Java String replace(), replaceFirst(), replaceAll() method example](https://www.javatpoint.com/java-string-replace)
- [String Manipulation in Java: Ultimate Guide](https://www.educative.io/collection/page/5642554087309312/5679846214598656/5701512225501184)