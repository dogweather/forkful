---
title:                "Écrire sur la sortie d'erreur standard"
html_title:           "Java: Écrire sur la sortie d'erreur standard"
simple_title:         "Écrire sur la sortie d'erreur standard"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi 

Écrire sur la sortie d'erreur standard en Java peut sembler être une tâche peu excitante, mais c'est en fait un outil utile pour déboguer et gérer les erreurs dans votre code. Cela vous permet également de mieux comprendre comment votre code fonctionne et comment il peut être amélioré.

## Comment faire 

Pour écrire sur la sortie d'erreur standard en Java, il vous suffit d'utiliser la méthode `System.err.println()` et d'insérer votre message entre les parenthèses. Par exemple :

```Java
System.err.println("Erreur : Vous devez saisir un nombre entier positif.");
```

Ceci imprimera votre message sur la sortie d'erreur standard, qui sera généralement affichée en rouge dans votre console. Voici un exemple de sortie :

```Java
Erreur : Vous devez saisir un nombre entier positif.
```

Vous pouvez également utiliser la méthode `System.err.print()` si vous souhaitez afficher un message sans sauter à la ligne suivante.

## Plongée en profondeur 

Lorsque vous écrivez sur la sortie d'erreur standard en Java, il est important de comprendre comment Java gère les erreurs. Par défaut, les erreurs sont imprimées sur la sortie d'erreur standard, mais il est possible de les rediriger vers un fichier ou même de les ignorer complètement en les gérant à l'aide de blocs `try` et `catch`.

Il est également important de noter que chaque fois que vous utilisez la méthode `System.err.println()`, un objet `PrintWriter` est créé pour gérer l'impression. Vous pouvez également utiliser cet objet directement pour plus de flexibilité dans la gestion de vos erreurs.

## Voir aussi 

- [Guide des exceptions et des erreurs en Java](https://www.jmdoudoux.fr/java/dej/chap-exceptions.htm)
- [Documentation officielle Java pour la classe System](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)