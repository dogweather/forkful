---
title:                "Clojure: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est essentiel pour un programmeur de vérifier si un répertoire existe pour s'assurer que son code fonctionne correctement et pour éviter les erreurs lors de l'accès à des fichiers.

## Comment faire

Pour vérifier si un répertoire existe en Clojure, vous pouvez utiliser la fonction `clojure.java.io/file?` en lui passant le chemin du répertoire en tant qu'argument. Voici un exemple de code : 

```Clojure
;; Vérifie si le répertoire existe
(clojure.java.io/file? "/chemin/vers/repertoire")
;; Renvoie true si le répertoire existe, sinon false
```

Si vous préférez utiliser une expression régulière pour vérifier si le répertoire existe, vous pouvez utiliser la fonction `clojure.java.io/re`. Voici un exemple :

```Clojure
;; Utilisation d'une expression régulière pour vérifier
;; si le répertoire existe
(clojure.java.io/re #".*repertoire.*" "/chemin/vers/repertoire")
;; Renvoie true si le répertoire existe, sinon false
```

## Plongée en profondeur

En réalité, la fonction `clojure.java.io/file?` ne vérifie pas vraiment si le répertoire existe, mais si l'objet passé en argument est un fichier existant ou un répertoire. Si l'objet est un répertoire, la fonction renverra toujours `true`. Pour vérifier si le répertoire existe réellement, il est recommandé d'utiliser une expression régulière.

## Voir aussi

- [Documentation sur la fonction `clojure.java.io/file?`](https://clojuredocs.org/clojure.java.io/file_q) 
- [Documentation sur la fonction `clojure.java.io/re`](https://clojuredocs.org/clojure.java.io/re)