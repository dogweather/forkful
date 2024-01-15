---
title:                "Lecture d'un fichier texte"
html_title:           "Fish Shell: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi lire un fichier texte?

Si vous aimez travailler avec la ligne de commande, alors vous avez sûrement entendu parler de Fish Shell, le shell de terminal convivial et moderne. L'une de ses fonctionnalités les plus utiles est la possibilité de lire et de manipuler des fichiers texte directement depuis le terminal. Dans cet article, nous allons vous montrer comment utiliser Fish Shell pour lire facilement un fichier texte, sans avoir à ouvrir un éditeur de texte séparé.

## Comment faire?

Voici un exemple simple de code qui utilise Fish Shell pour lire un fichier texte:

```Fish Shell
cat fichier.txt
```
En utilisant la commande `cat`, nous pouvons afficher le contenu du fichier texte directement dans le terminal. Mais que faire si nous voulons seulement afficher une partie spécifique du fichier?

```Fish Shell
head -5 fichier.txt
```
Avec la commande `head`, nous pouvons spécifier le nombre de lignes que nous voulons afficher. Dans cet exemple, nous affichons les 5 premières lignes du fichier.

Nous pouvons également utiliser la commande `tail` pour afficher les dernières lignes d'un fichier. De plus, nous pouvons utiliser des expressions régulières pour effectuer des recherches spécifiques dans un fichier texte.

## Plongée en profondeur

La commande `cat` que nous avons utilisée précédemment combine plusieurs fichiers en un seul. Si nous voulons uniquement afficher le contenu d'un fichier sans le fusionner avec un autre, nous pouvons utiliser la commande `less`. Cette commande nous permet également de naviguer dans le fichier à l'aide des touches fléchées du clavier.

De plus, Fish Shell offre des fonctionnalités avancées pour lire et manipuler des fichiers texte, telles que la possibilité de créer des alias pour des commandes couramment utilisées et des modules complémentaires pour ajouter de nouvelles fonctionnalités.

# Voir aussi

Pour en savoir plus sur les fonctionnalités de Fish Shell, vous pouvez consulter la documentation officielle et les forums de la communauté. Vous pouvez également explorer les différents modules complémentaires disponibles pour améliorer votre expérience avec Fish Shell.

- Documentation Fish Shell: https://fishshell.com/docs/current/index.html
- Communauté Fish Shell: https://fishshell.com/community.html
- Modules complémentaires Fish Shell: https://github.com/fishery/fishery