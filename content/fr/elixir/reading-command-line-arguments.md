---
title:                "Elixir: La lecture des arguments de ligne de commande"
simple_title:         "La lecture des arguments de ligne de commande"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Les arguments de ligne de commande sont un élément essentiel de la programmation en Elixir. Dans cet article, nous allons explorer pourquoi la lecture des arguments de ligne de commande est importante et comment le faire efficacement.

## Comment faire

La lecture des arguments de ligne de commande en Elixir est un processus simple et rapide. Tout d'abord, nous devons inclure le module `OptionParser` dans notre code :

```Elixir
import OptionParser
```

Ensuite, nous pouvons définir nos options et spécifier les valeurs attendues :

```Elixir
OptionParser.parse(
  [ "-f", "--file NAME", :string ],
  [ "-v", "--verbose", :boolean ],
  [ "-h", "--help" ]
)
```

Dans cet exemple, nous définissons trois options : `--file` pour spécifier le nom du fichier, `--verbose` pour activer le mode bavard et `--help` pour afficher l'aide. Nous pouvons également définir une liste d'options par défaut à utiliser si aucun argument n'est fourni.

Ensuite, nous pouvons récupérer les arguments en utilisant la fonction `parse` de `OptionParser` :

```Elixir
{ arguments, options } = OptionParser.parse!(System.argv)
```

Nous pouvons ensuite accéder aux valeurs des arguments grâce à `options` ou `arguments` en fonction de notre implémentation.

## Deep Dive

La raison pour laquelle la lecture des arguments de ligne de commande est importante en Elixir est qu'elle nous permet de fournir des options à notre programme au moment de l'exécution. Cela signifie que nous pouvons personnaliser le comportement de notre programme en fonction des données fournies par l'utilisateur.

De plus, la lecture des arguments de ligne de commande est un moyen efficace de traiter les arguments de manière structurée. Grâce à `OptionParser`, nous pouvons spécifier le type de données attendu pour chaque option, ce qui facilite la manipulation des arguments dans notre code.

## Voir aussi

- [Documentation d'OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
- [Blog post sur la manipulation des arguments de ligne de commande en Elixir](https://www.pluralsight.com/guides/handling-command-line-options-elixir)

Merci d'avoir lu ! N'hésitez pas à explorer d'autres fonctionnalités d'Elixir pour améliorer votre expérience de programmation.