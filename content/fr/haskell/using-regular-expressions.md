---
title:    "Haskell: Utiliser les expressions régulières"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

##Pourquoi

Les expressions régulières sont un outil puissant pour traiter et manipuler du texte dans vos programmes Haskell. Elles permettent de trouver et de remplacer des motifs spécifiques dans une chaîne de caractères en utilisant des règles précises. Les expressions régulières sont particulièrement utiles pour valider et filtrer les données d'entrée utilisateur, ainsi que pour extraire des informations spécifiques à partir de grandes quantités de texte.

##Comment faire

Pour utiliser les expressions régulières en Haskell, nous devons d'abord importer le module `Text.Regex.Posix` dans notre fichier. Ensuite, nous pouvons utiliser la fonction `makeRegex` pour créer une expression régulière à partir d'un motif spécifique. Par exemple, si nous voulons trouver toutes les adresses e-mail dans une chaîne de caractères, nous pouvons utiliser l'expression régulière suivante: `makeRegex "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,6}$"`.

Ensuite, nous pouvons utiliser la fonction `match` pour rechercher des correspondances spécifiques dans une chaîne de caractères en utilisant notre expression régulière. Par exemple, si nous avons une chaîne de caractères contenant plusieurs adresses e-mail, nous pouvons utiliser `match (makeRegex "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,6}$") "Mon adresse email est john@doe.com, vous pouvez me contacter à jane@doe.com"` pour trouver et renvoyer une liste contenant les deux adresses e-mail.

Dans le cas où il n'y a pas de correspondance pour l'expression régulière donnée dans une chaîne de caractères, la fonction `match` renverra `Nothing`.

##Deep Dive

Les expressions régulières en Haskell suivent la même syntaxe que celles utilisées dans d'autres langages, comme Perl ou Python. Il y a cependant quelques différences à noter. Par exemple, l'utilisation de l'opérateur `~` n'est pas nécessaire avant une expression régulière en Haskell, car elle est déjà implicite. De plus, la plupart des fonctions de manipulation de texte en Haskell, telles que `lines` ou `words`, ignorent automatiquement les caractères invisibles lorsqu'elles sont utilisées avec des expressions régulières.

##Voir aussi

- [Documentation officielle de `Text.Regex.Posix`](https://hackage.haskell.org/package/regex-posix)
- [Un tutoriel complet sur la manipulation de texte en Haskell](https://www.fpcomplete.com/blog/2017/11/string-manipulation-in-haskell/)
- [Guide pratique pour les expressions régulières en Haskell](https://wiki.haskell.org/Regular_expressions)