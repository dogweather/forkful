---
title:    "Elixir: Commencer un nouveau projet"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes un développeur Elixir passionné ou simplement curieux d'en apprendre davantage sur ce langage de programmation, vous êtes au bon endroit. Dans cet article, nous allons plonger dans les bases de la création d'un nouveau projet en Elixir.

## Comment faire 

Pour commencer, il est important de noter que Elixir est un langage fonctionnel axé sur la concurrence. Cela signifie qu'il est idéal pour créer des applications évolutives et résistantes aux pannes. Pour créer un nouveau projet en Elixir, il suffit de suivre ces étapes simples :

```
Elixir mix new nom_du_projet
```

Cela créera un nouveau projet avec une structure de fichiers initiale et un fichier `mix.exs` qui contient les dépendances et les tâches de construction du projet.

Ensuite, vous pouvez ajouter des dépendances supplémentaires telles que des packages utiles à votre projet en utilisant la commande `mix deps.get`, qui mettra à jour le fichier `mix.lock`.

Une fois que vous avez votre projet en place, il est temps de commencer à coder. Voici un exemple d'une fonction qui prend un nombre en entrée et renvoie le double de ce nombre :

```
defp doubler(nombre) do 
  nombre * 2
end 

IO.puts doubler(5)
```
Voici la sortie que nous obtiendrons en exécutant la fonction :

```
10
```

Vous pouvez également utiliser Elixir pour créer une interface de ligne de commande pour votre projet en utilisant le module `OptionParser`. Voici un exemple de commande qui prend une option en ligne de commande et renvoie un message en conséquence :

```
OptionParser.parse(["--message", "Bonjour"]) do 
  arg, val -> 
    IO.puts("votre message est : #{val}")
end
```
Voici la sortie que nous obtiendrons en exécutant cette commande :

```
votre message est : Bonjour
```

## Approfondissement 

Maintenant que vous avez une idée de base de la création d'un nouveau projet en Elixir, vous pouvez approfondir vos connaissances en explorant des concepts tels que les fonctionnalités avancées de la programmation fonctionnelle, la manipulation de la concurrence et la mise en œuvre de tests unitaires. Vous pouvez également consulter la documentation complète d'Elixir pour plus d'informations sur les différents modules et fonctions disponibles.

## Voir aussi

Voici quelques liens utiles pour continuer votre apprentissage d'Elixir :

- [Guide officiel Elixir](https://elixir-lang.org/getting-started/introduction.html)
- [Documentation complète Elixir](https://hexdocs.pm/elixir/Kernel.html)
- [Site web de la communauté Elixir](https://elixir-lang.org/community.html)
- [Exemples de projets Elixir](https://github.com/h4cc/awesome-elixir)