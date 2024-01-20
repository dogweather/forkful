---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

La recherche et le remplacement de texte permettent de repérer une chaîne de caractères spécifique et de la remplacer par une autre. Les programmeurs l'utilisent pour modifier rapidement les codes source ou configurer les systèmes.

## Comment faire :

Utiliser le shell de poisson(Fish Shell) pour la recherche et le remplacement est facile. Jetons un œil à cet exemple.

```Fish Shell 
set sentence "J'aime le poisson"
echo $sentence | string replace "poisson" "shell de poisson"
``` 

Sortie: "J'aime le shell de poisson"

Ici, nous avons remplacé 'poisson' par 'shell de poisson' dans la variable sentence.

```Fish Shell
set list "un deux trois un deux trois"
echo $list | string replace --all "un" "one"
```
Sortie: "one deux trois one deux trois"

Dans cet exemple, nous avons remplacé toutes les instances de 'un' par 'one' dans la variable list.

## Plongée profonde :

Le shell de poisson a été initialement développé pour être plus intuitif et facile à utiliser que les coquilles traditionnelles. D'autres alternatives existent, comme grep et sed pour la recherche et le remplacement de texte, mais le Shell de poisson rend les choses beaucoup plus simples.

Le détail d'implémentation important ici est que le Shell de poisson utilise son propre language de script, qui est différent des shells UNIX traditionnels. Cela signifie qu'il est plus facile à utiliser et plus flexible pour faire des tâches comme la recherche et le remplacement de texte.

## Voir aussi :

Pour plus d'informations sur le poisson Shell et ses capacités, jetez un oeil à ces liens:
1. [Documentation officielle de poisson Shell](https://fishshell.com/docs/current/index.html)
2. [Github du projet poisson Shell](https://github.com/fish-shell/fish-shell)
3. [Tutoriel poisson Shell](https://fishshell.com/docs/current/tutorial.html)