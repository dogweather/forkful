---
title:    "Clojure: La concaténation de chaînes de caractères"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes est une pratique courante en programmation qui consiste à fusionner plusieurs chaînes de caractères en une seule. Cela peut être utile dans de nombreuses situations, comme l'affichage de données à l'utilisateur ou la création de noms de fichiers dynamiques.

## Comment faire

La concaténation de chaînes en Clojure est très simple grâce à la fonction `str`. Il suffit de mettre toutes les chaînes que vous souhaitez fusionner en arguments de cette fonction et de la placer dans une des expressions `println` ou `format` pour l'afficher à l'écran. Par exemple :

```Clojure
(println (str "Bonjour" ", " "monde!"))
```

Donne en sortie :

```
Bonjour, monde!
```


## Plongée en profondeur

En plus de la fonction `str`, Clojure offre également la fonction `join` pour concaténer des chaînes avec un délimiteur spécifique. De plus, la concaténation de chaînes peut également être réalisée avec l'opérateur `+`. Cependant, il est déconseillé de l'utiliser avec un grand nombre de chaînes, car cela peut entraîner des performances médiocres.

Il est également important de noter que les chaînes en Clojure sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées une fois qu'elles ont été créées. Cela peut être surprenant pour les programmeurs habitués à des langages où les chaînes sont mutables, comme Java.

## Voir aussi

- [Documentation officielle pour la fonction `str`](https://clojuredocs.org/clojure.core/str)
- [Documentation officielle pour la fonction `join`](https://clojuredocs.org/clojure.core/join)