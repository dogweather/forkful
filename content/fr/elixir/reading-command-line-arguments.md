---
title:                "Elixir: Lecture des arguments de ligne de commande"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous écrivez du code Elixir, il est important de savoir comment lire les arguments de ligne de commande. Cela peut sembler être une petite fonctionnalité, mais cela peut entraîner une grande amélioration de votre expérience de programmation en rendant votre code plus flexible et en vous permettant de le personnaliser en fonction de différentes entrées.

## Comment faire

Pour lire les arguments de ligne de commande en Elixir, nous pouvons utiliser la fonction `System.argv/0`. Cette fonction renverra une liste de tous les arguments passés lors de l'exécution de notre programme. Voyons cela en action avec un exemple de code :

```Elixir
# Définir une fonction pour afficher les arguments
def print_args do
  # En utilisant la fonction System.argv/0 pour obtenir la liste d'arguments
  args = System.argv()
  # Afficher chaque argument un par un
  Enum.each(args, fn arg ->
      IO.puts(arg)
  end)
end

# Appeler notre fonction
print_args()
```

Supposons que nous enregistrions ce code dans un fichier `args.exs`. Nous pouvons alors l'exécuter en utilisant la commande suivante dans notre terminal :

```
elixir args.exs argument1 argument2 argument3
```
Et voici la sortie que nous obtiendrons :

```
$ elixir args.exs argument1 argument2 argument3

argument1
argument2
argument3
```

Nous pouvons également utiliser `System.argv/1` pour obtenir un argument spécifique en fonction de son index dans la liste. Par exemple, si nous voulons obtenir seulement le deuxième argument, nous pouvons utiliser `System.argv(1)`, car les indices de liste commencent à partir de 0. Voici un autre exemple de code :

```Elixir
def print_second_arg do
  # En utilisant la fonction System.argv/1 pour obtenir le deuxième argument
  second_arg = System.argv(1)
  # Afficher le deuxième argument
  IO.puts(second_arg)
end

print_second_arg()
```

Et voici la sortie correspondante :

```
$ elixir args.exs argument1 argument2 argument3

argument2
```

## Plongée en profondeur

Il est également important de noter qu'il existe des bibliothèques externes en Elixir qui peuvent vous aider à gérer les arguments de ligne de commande de manière plus avancée. Par exemple, la bibliothèque `Jason` permet de lire facilement les arguments passés sous forme de fichiers JSON.

De plus, Elixir permet également de définir des options d'exécution personnalisées en utilisant le module `OptionParser`. Cela peut être utile pour créer des programmes plus complexes avec plusieurs fonctionnalités.

## Voir aussi

- [Documentation Elixir officielle sur System.argv/0](https://hexdocs.pm/elixir/System.html#argv/0)
- [Documentation Elixir officielle sur System.argv/1](https://hexdocs.pm/elixir/System.html#argv/1)
- [Bibliothèque Jason pour gérer les arguments JSON en Elixir](https://github.com/michalmuskala/jason)
- [Module OptionParser en Elixir](https://hexdocs.pm/elixir/OptionParser.html)