---
title:                "Créer un fichier temporaire"
html_title:           "Clojure: Créer un fichier temporaire"
simple_title:         "Créer un fichier temporaire"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer des fichiers temporaires peut sembler être une tâche simple et sans importance, mais c'est en fait une étape essentielle pour de nombreux programmes. Les fichiers temporaires permettent aux applications de stocker temporairement des données, d'exécuter des opérations ou de partager des informations entre différents processus.

## Comment faire

Pour créer un fichier temporaire en Clojure, nous pouvons utiliser la fonction standard`with-open`. Cela nous permet d'ouvrir un flux de sortie vers un fichier et de l'utiliser dans une expression. Une fois l'expression terminée, le flux se ferme automatiquement et le fichier temporaire est effacé. Voici un exemple de code:

```Clojure
(with-open [f (java.io.File/createTempFile "prefix" "suffix")]
  (println "Le chemin du fichier temporaire est:" (.getAbsolutePath f)))
```

Cela créera un fichier temporaire avec un préfixe "prefix" et un suffixe "suffix" et en affichera le chemin absolu. Une fois le code exécuté, le fichier sera automatiquement effacé.

## Plongée en profondeur

Dans l'exemple précédent, nous avons utilisé la fonction`createTempFile` de la classe Java`java.io.File` pour créer un fichier temporaire. Cette fonction prend en paramètres un préfixe et un suffisant pour générer un nom de fichier unique. Cependant, si vous avez besoin d'un contrôle plus précis sur la création du fichier temporaire, vous pouvez également utiliser la fonction`createTempFile` de la classe`java.nio.file.Files`.

Cette fonction prend en paramètres un répertoire parent et des options pour la création du fichier, telles que le préfixe, le suffixe, le type de fichier ou les autorisations. Elle renvoie un objet`Path` qui peut ensuite être utilisé pour ouvrir un flux vers le fichier temporaire.

## Voir aussi

- [Documentation officielle de Clojure sur la fonction`with-open`](https://clojuredocs.org/clojure.core/with-open)
- [Documentation officielle de Java sur`java.io.File`](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Documentation officielle de Java sur`java.nio.file.Files`](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)