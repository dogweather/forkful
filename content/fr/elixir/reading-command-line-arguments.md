---
title:    "Elixir: La lecture des arguments de ligne de commande"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi

Les arguments de ligne de commande sont une fonctionnalité importante de tout langage de programmation, y compris Elixir. Ils permettent aux utilisateurs de spécifier des options et des paramètres lorsqu'ils exécutent un programme à partir de la ligne de commande. Dans cet article, nous allons explorer pourquoi la lecture des arguments de ligne de commande est essentielle dans la programmation Elixir.

# Comment Faire

Pour lire les arguments de ligne de commande en Elixir, nous utilisons le module `System` et sa fonction `argv/0`. Cette fonction retourne une liste contenant tous les arguments passés lors de l'exécution du programme. Voici un exemple de code pour lire et afficher les arguments de ligne de commande :

```Elixir
liste_arguments = System.argv()
IO.puts("Les arguments de ligne de commande sont : #{inspect liste_arguments}")
```

Supposons que vous exécutez ce code avec les arguments `hello world`. Le résultat affichera :

```
Les arguments de ligne de commande sont : ["hello", "world"]
```

Cela montre que les arguments de ligne de commande sont stockés sous forme de chaînes de caractères dans une liste.

# Plongée Profonde

Outre la fonction `argv/0`, le module `System` fournit également d'autres fonctions utiles pour travailler avec les arguments de ligne de commande. Par exemple, `get_env/2` vous permet de récupérer des valeurs d'environnement basées sur les arguments passés. La fonction `get_env/2` prend deux arguments : le nom de l'environnement et une valeur par défaut à utiliser si l'environnement n'est pas défini. Voici un exemple :

```Elixir
# Si la valeur de l'environnement PORT est définie, elle est assignée à la variable "port".
# Sinon, la valeur par défaut de 8080 est utilisée.
port = System.get_env("PORT", 8080)
```

Dans cet exemple, si vous exécutez le programme avec l'argument `PORT=3000`, alors la variable `port` sera assignée à la valeur `3000`.

Les arguments de ligne de commande peuvent également être utilisés pour exécuter différentes tâches en fonction des options spécifiées. Par exemple, vous pouvez utiliser des options telles que `-h` pour afficher l'aide ou `-v` pour afficher la version de votre programme.

# Voir Aussi

Pour en savoir plus sur la lecture des arguments de ligne de commande en Elixir, consultez les ressources suivantes :

- Documentation officielle d'Elixir sur les arguments de ligne de commande : https://hexdocs.pm/elixir/System.html
- Un tutoriel sur les arguments de ligne de commande en Elixir : https://www.codementor.io/elixir/tutorial/how-to-use-command-line-argument-in-elixir
- Un article sur les meilleures pratiques pour utiliser les arguments de ligne de commande : https://medium.com/@ElixirShots/elixir-tips-arguments-from-the-command-line-c39b722bc124