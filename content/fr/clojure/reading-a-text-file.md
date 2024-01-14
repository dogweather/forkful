---
title:    "Clojure: Lecture d'un fichier texte"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi

La lecture de fichiers texte est une tâche courante dans la programmation Clojure. Il est important de savoir comment le faire efficacement et facilement. Dans cet article, nous allons apprendre comment lire un fichier texte en utilisant Clojure.

# Comment faire

Nous allons d'abord créer un fichier texte avec du contenu à lire. Nous pouvons le faire en utilisant la commande ```touch``` dans notre terminal (si nous utilisons un système d'exploitation Unix), ou en utilisant un éditeur de texte tel que Notepad++ ou Sublime Text (pour les systèmes d'exploitation Windows).

Dans notre fichier texte, nous allons simplement ajouter quelques lignes de texte pour que nous puissions les lire plus tard. Par exemple :

```
Bonjour à tous !
Je suis un fichier texte en Clojure.
J'adore être lu par des programmes passionnants.
Au revoir !
```

Maintenant que nous avons notre fichier texte, nous pouvons passer à la partie Clojure. Nous allons utiliser la fonction ```slurp``` pour lire le contenu du fichier texte et le stocker dans une variable. Voici le code que nous utiliserons :

```
(def contenu (slurp "chemin/vers/mon/fichier/texte.txt"))
```

Nous avons maintenant stocké le contenu de notre fichier texte dans la variable ```contenu```. Nous pouvons vérifier en imprimant le contenu avec la fonction ```println``` :

```
(println contenu)
```

Cela devrait donner la sortie suivante :

```
Bonjour à tous !
Je suis un fichier texte en Clojure.
J'adore être lu par des programmes passionnants.
Au revoir !
```

Et voilà, nous avons réussi à lire notre fichier texte avec succès !

# Plongée en profondeur

Il est important de noter que la fonction ```slurp``` lit tout le contenu du fichier en une seule fois et le stocke en mémoire. Cela peut poser un problème si notre fichier texte est très volumineux et pourrait entraîner des problèmes de performances. Dans ce cas, il serait préférable d'utiliser la fonction ```line-seq``` qui lit le fichier ligne par ligne au lieu de tout en une fois.

De plus, si le fichier texte contient des caractères spéciaux ou des caractères non ASCII, il est recommandé d'utiliser la fonction ```with-open``` pour s'assurer que le fichier est fermé correctement après la lecture.

# Voir aussi

- Documentation officielle de Clojure sur la fonction ```slurp``` : https://clojuredocs.org/clojure.core/slurp
- Documentation officielle de Clojure sur la fonction ```line-seq``` : https://clojuredocs.org/clojure.core/line-seq
- Documentation officielle de Clojure sur la fonction ```with-open``` : https://clojuredocs.org/clojure.core/with-open