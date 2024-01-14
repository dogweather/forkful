---
title:    "Java: Suppression des caractères correspondant à un motif"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondants à un modèle peut être une tâche utile lors de la manipulation de chaînes de caractères en Java. Cela peut être particulièrement pratique lors de la suppression de données inutiles ou non désirées dans un texte.

## Comment Faire

Pour supprimer des caractères correspondants à un modèle en Java, vous pouvez utiliser la méthode `replaceAll()` de la classe `String`. Cette méthode prend deux paramètres : le premier est le modèle que vous voulez supprimer, et le deuxième est le string de remplacement. Voici un exemple de code :

```Java
String phrase = "J'aime les fruits exotiques comme les mangues et les ananas.";
String remplacement = "";
String resultat = phrase.replaceAll("o", remplacement);
System.out.println(resultat);
```

L'exemple ci-dessus va supprimer tous les caractères "o" dans la phrase et imprimer le résultat suivant : "J'aime les fruits ex­tiques cmme les manges et les ananas." En utilisant une chaîne vide comme remplacement, le caractère cible est simplement supprimé.

## Profondeur d'analyse

En utilisant la méthode `replaceAll()`, vous pouvez également utiliser des expressions régulières pour supprimer des caractères correspondants à des motifs plus complexes. Par exemple, si vous voulez supprimer tous les chiffres d'une chaîne de caractères, vous pouvez utiliser l'expression régulière `[0-9]` comme modèle et une chaîne vide comme remplacement.

Voici un autre exemple :

```Java
String phrase = "Il y a 24 heures dans une journée";
String remplacement = "";
String resultat = phrase.replaceAll("[0-9]", remplacement);
System.out.println(resultat);
```

Le résultat sera : "Il y a heures dans une journée".

## Voir Aussi

- [Documentation de la méthode `replaceAll()` en Java](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#replaceAll(java.lang.String,%20java.lang.String))
- [Guide sur les expressions régulières en Java](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Exemples pratiques de manipulation de chaînes en Java](https://www.baeldung.com/java-string-manipulation)