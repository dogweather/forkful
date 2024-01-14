---
title:    "Fish Shell: Concaténation de chaînes de caractères"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une pratique courante dans la programmation. Elle permet de combiner plusieurs chaînes de caractères pour en former une seule, ce qui peut être utile pour créer des noms de fichiers dynamiques, des messages personnalisés et bien plus encore.

## Comment faire

Pour concaténer des chaînes de caractères en utilisant le Shell Fish, vous pouvez utiliser l'opérateur `+` ou la fonction `string join`. Voici un exemple de code :

```Fish Shell
set name "Julie"
echo "Bonjour" + $name
```

Cela affichera `Bonjour Julie`. Vous pouvez également utiliser la fonction `string join` pour concaténer plus de deux chaînes de caractères :

```Fish Shell
set fruits "pomme" "banane" "orange"
echo (string join ", " $fruits)
```

Cela affichera `pomme, banane, orange`.

## Plongée en profondeur

Il existe des méthodes plus avancées pour concaténer des chaînes de caractères, telles que l'utilisation du tableau `string` ou des fonctions `string trim` et `string split`. Cela peut être utile si vous travaillez avec des chaînes de caractères plus complexes.

La concaténation de chaînes de caractères peut également être effectuée de manière efficace en utilisant des boucles, des conditions et des expressions régulières. N'hésitez pas à explorer ces options pour trouver la méthode qui convient le mieux à votre besoin.

## Voir aussi

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/)
- [Guide de référence rapide pour Fish Shell en français](https://fishshell.com/docs/current/index-fr.html)
- [Tutoriel pour débuter avec Fish Shell](https://dev.to/rakhmanarikan/get-started-with-fish-shell-5cl2)