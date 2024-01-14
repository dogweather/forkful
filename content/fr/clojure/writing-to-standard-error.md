---
title:                "Clojure: Écrire vers l'erreur standard"
simple_title:         "Écrire vers l'erreur standard"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire sur la sortie d'erreur standard (standard error) peut sembler effrayant au premier abord, mais c'est en fait un concept très utile à comprendre pour tout programmeur Clojure. Cet article explique pourquoi et comment utiliser la sortie d'erreur standard dans votre code Clojure.

## Comment Faire

Utiliser la sortie d'erreur standard n'est pas aussi complexe qu'il n'y paraît. Tout d'abord, il faut comprendre la différence entre la sortie standard (standard output) et la sortie d'erreur standard. La sortie standard est utilisée pour afficher les résultats et les informations de votre programme, tandis que la sortie d'erreur standard est utilisée pour afficher les erreurs et les avertissements.

Pour écrire sur la sortie d'erreur standard, vous pouvez utiliser la fonction `System/err.println`. Voici un exemple de code avec sa sortie correspondante :

```Clojure
(System/err.println "Il y a une erreur!")
```

```
Il y a une erreur!
```

Comme vous pouvez le voir, la phrase "Il y a une erreur!" est affichée sur la sortie d'erreur standard. Vous pouvez également utiliser la macro `println` pour écrire sur la sortie d'erreur standard :

```Clojure
(println System/err "Il y a une erreur!")
```

## Plongée Profonde

Bien que l'utilisation de la sortie d'erreur standard puisse sembler assez simple, il y a des nuances à prendre en compte. Tout d'abord, vous pouvez rediriger la sortie d'erreur standard vers un fichier en utilisant la commande `clojure.core/with-out-str`. Cela peut être utile lorsque vous voulez enregistrer les erreurs et les avertissements dans un journal.

De plus, vous pouvez personnaliser le format dans lequel les erreurs et les avertissements sont affichés en utilisant la macro `with-err-str`. Cela peut être utile lorsque vous voulez capturer les erreurs et les traiter d'une manière spécifique.

## Voir Aussi

- [Documentation officielle de Clojure sur la sortie d'erreur standard](https://clojure.org/reference/repl_and_main#_redirects_and_output_capture)
- [Article de blog sur l'utilisation de la sortie d'erreur standard en Clojure](https://clojuredocs.org/clojure.core/with-out-str)
- [Vidéo explicative sur la sortie d'erreur standard en Clojure](https://www.youtube.com/watch?v=jmQjUepBUmg)