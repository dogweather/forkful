---
title:    "Fish Shell: Traduction: Lecture d'un fichier texte"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

Lire et manipuler des fichiers texte est une tâche courante pour les programmeurs. Que vous souhaitiez extraire des données, effectuer des recherches ou simplement lire le contenu d'un fichier, savoir comment le faire en utilisant Fish Shell peut être un atout précieux dans votre boîte à outils de programmation.

## Comment faire

Pour lire un fichier texte avec Fish Shell, vous pouvez utiliser la commande `cat`. Par exemple, si nous voulons lire le contenu du fichier `mon_fichier.txt`, nous utiliserions la commande suivante dans notre terminal :

```Fish Shell
cat mon_fichier.txt
```

Cela affichera le contenu du fichier directement dans notre terminal. Si nous voulons faire quelque chose avec ce contenu (par exemple, le stocker dans une variable ou le manipuler d'une certaine manière), nous pouvons utiliser le rediriger avec le symbole `>`. Par exemple, si nous voulons stocker le contenu du fichier dans une variable nommée `contenu`, nous utiliserions la commande suivante :

```Fish Shell
contenu = (cat mon_fichier.txt)
```

Nous pouvons également utiliser la commande `less` pour lire un fichier texte. Cette commande permet de naviguer dans le contenu du fichier en utilisant les touches fléchées de notre clavier. Par exemple, si nous voulons lire le fichier `mon_fichier.txt`, nous utiliserions la commande suivante :

```Fish Shell
less mon_fichier.txt
```

## Plongée en profondeur

En utilisant Fish Shell, il est également possible de lire un fichier texte ligne par ligne et de faire des manipulations sur chaque ligne individuelle. Pour ce faire, nous pouvons utiliser la commande `while read` combinée à la commande `echo`. Par exemple, si nous voulons lire chaque ligne du fichier `mon_fichier.txt` et afficher le résultat dans le terminal, nous pouvons utiliser le code suivant :

```Fish Shell
while read ligne
    echo $ligne
end < mon_fichier.txt
```

Cela parcourra le fichier ligne par ligne et affichera chaque ligne dans le terminal. Vous pouvez également effectuer des manipulations supplémentaires sur chaque ligne en utilisant des commandes supplémentaires à l'intérieur de la boucle `while`.

## Voir aussi

- [La documentation de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Un guide pratique pour débuter avec Fish Shell](https://dev.to/herodotengineer/getting-started-with-fish-shell-1jin)
- [Un tutoriel en français sur Fish Shell](https://blog.chipster.fr/fish-shell/)

Merci d'avoir lu cet article sur la lecture de fichiers texte avec Fish Shell. En utilisant ces techniques, vous pourrez facilement lire et manipuler des fichiers texte dans votre programmation quotidienne. N'hésitez pas à explorer davantage Fish Shell et à découvrir toutes ses fonctionnalités puissantes !