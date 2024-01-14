---
title:    "Clojure: Vérifier si un répertoire existe"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Pourquoi

Dans le monde de la programmation, il est essentiel de pouvoir vérifier si un dossier existe avant de travailler avec celui-ci. Cela permet de s'assurer que notre code fonctionnera correctement et évite les erreurs et les bugs potentiels.

# Comment faire

La bonne nouvelle est que grâce à Clojure, il est très facile de vérifier si un dossier existe. Utilisons la fonction ```clojure (.exists (file "nom_dossier"))``` pour vérifier si le dossier nommé "nom_dossier" existe ou non. Cette fonction renverra une valeur booléenne (true ou false) en fonction du résultat de la vérification.

Voici un exemple de code pour illustrer cela :

```clojure
(def dossier (file "test"))
(.mkdir dossier)
(.exists dossier) ; renverra true car le dossier "test" vient d'être créé
```

Et voici le résultat de l'execution du code :

```clojure
true
```

En utilisant cette fonction, nous pouvons facilement implémenter des conditions pour gérer les cas où le dossier existe ou n'existe pas.

# Plongée en profondeur

En creusant un peu plus, nous pouvons également explorer d'autres méthodes utiles pour travailler avec des dossiers en Clojure, telles que :

- ```clojure (.listFiles (file "nom_dossier"))``` : renvoie une liste de tous les fichiers contenus dans le dossier spécifié.
- ```clojure (.list (file "nom_dossier"))``` : renvoie une liste de tous les fichiers et sous-dossiers contenus dans le dossier spécifié.
- ```clojure (.delete (file "nom_dossier"))``` : supprime le dossier spécifié.

Grâce à ces fonctions, nous pouvons facilement gérer et manipuler les dossiers dans notre code en utilisant Clojure.

# Voir aussi

- Document officiel sur les fonctions de manipulation de fichiers en Clojure : https://clojure.org/reference/java_interop#_manipulating_files
- Tutoriel sur la manipulation de fichiers en Clojure : https://www.baeldung.com/clojure-files