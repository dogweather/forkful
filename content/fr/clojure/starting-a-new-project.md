---
title:                "Clojure: Lancer un nouveau projet"
programming_language: "Clojure"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Pourquoi
Nous avons tous été à un moment donné inspirés pour commencer un nouveau projet de programmation, que ce soit pour apprendre une nouvelle langue de programmation ou pour répondre à un besoin spécifique. Dans cet article, nous allons expliquer comment démarrer un nouveau projet Clojure afin que vous puissiez sauter dans l'action dès maintenant !

# Comment Faire
Pour commencer, vous avez besoin d'installer Clojure sur votre ordinateur. Si vous utilisez un système d'exploitation basé sur Unix, vous pouvez simplement utiliser le gestionnaire de paquets pour l'installer. Sinon, vous pouvez télécharger l'archive depuis le site officiel de Clojure.

Une fois Clojure installé, ouvrez votre terminal et exécutez la commande `lein new` pour créer un nouveau projet. Donnez un nom à votre projet et vous voilà prêt à partir !

Passons maintenant à un exemple de code pour créer une fonction simple qui calcule la somme de deux nombres :

```
(defn sum [a b]
  (+ a b))

(println (sum 3 4))
```

Cela devrait afficher `7` dans votre terminal en exécutant le fichier. Vous pouvez maintenant ajouter d'autres fonctions et commencer à écrire du code pour votre projet.

# Plongée en Profondeur
En démarrant un nouveau projet Clojure, il peut être utile de suivre quelques bonnes pratiques pour un développement efficace. Tout d'abord, il est recommandé d'utiliser un dépôt de contrôle de version comme Git pour enregistrer votre code et suivre son évolution. Deuxièmement, vous pouvez utiliser des outils de gestion de dépendances comme Leiningen pour faciliter l'installation et la mise à jour des bibliothèques utilisées par votre projet.

Il est également important de suivre la convention de nommage des fonctions et des variables en Clojure. Les noms de fonctions devraient être verbes et les noms de variables des noms descriptifs de ce qu'ils représentent. Enfin, prenez le temps de documenter votre code pour vous et pour les autres développeurs qui pourraient travailler sur le projet à l'avenir.

# Voir Aussi
- Le site officiel de Clojure : https://clojure.org/
- Un tutoriel pour démarrer avec Clojure : https://kleinschmidt.blog/primitive-web-technologies-in-clojure/
- Un guide de style pour la programmation en Clojure : https://gist.github.com/bbatsov/3b4b7741c5f3ef0172e0