---
title:    "Clojure: Commencer un nouveau projet"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes intéressé(e) par la programmation fonctionnelle et que vous cherchez un langage simple et élégant pour vos projets, alors Clojure pourrait être le choix parfait pour vous.

# Comment faire

```Clojure
(defn greet [name]
  (println (str "Bonjour " name " !")))
```

La première étape pour commencer un nouveau projet Clojure est d'installer l'environnement de développement. Vous pouvez utiliser Leiningen ou Boot pour gérer vos dépendances et compiler votre code. Une fois cela fait, vous pouvez commencer à écrire du code avec la syntaxe simple et concise de Clojure.

Dans l'exemple ci-dessus, nous avons créé une fonction appelée "greet" qui prend en paramètre un nom et affiche un message de salutation en utilisant la fonction "println".

```Clojure
(greet "Marie")
```

Output: Bonjour Marie !

# Plongée en profondeur

Clojure est basé sur le langage Lisp et fonctionne sur la machine virtuelle Java. Cela signifie qu'il peut profiter de la puissance et des bibliothèques de Java tout en offrant une syntaxe plus agréable à utiliser. De plus, Clojure est un langage immuable, ce qui signifie que les données ne peuvent pas être modifiées après avoir été créées, ce qui rend le code plus sûr et plus facile à comprendre.

Pour créer un nouveau projet Clojure, vous pouvez utiliser le template fourni par Leiningen pour générer automatiquement la structure de base du projet. Vous pouvez ensuite ajouter vos propres fichiers et commencer à coder !

# Voir aussi

- [Documentation officielle Clojure](https://clojure.org/)
- [Leiningen](https://leiningen.org/)
- [Boot](https://boot-clj.com/)