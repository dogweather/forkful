---
title:                "Elixir: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Que vous soyez un développeur débutant ou expérimenté, travailler avec des fichiers CSV est une compétence précieuse à avoir dans votre boîte à outils de programmation. Les fichiers CSV sont un moyen simple et efficace de stocker et d'échanger des données tabulaires, ce qui en fait un format de fichier couramment utilisé dans de nombreux domaines, tels que la finance, les statistiques et les sciences de données.

## Comment faire

Pour travailler avec des fichiers CSV en utilisant Elixir, nous pouvons utiliser la bibliothèque `CSV` incluse dans le langage. Tout d'abord, nous devons l'ajouter à notre projet en ajoutant `:csv` à notre liste de dépendances dans le fichier `mix.exs`. 

Ensuite, nous pouvons utiliser la fonction `CSV.parse/2` pour lire un fichier CSV et renvoyer une liste de lignes. Par exemple, si nous avons un fichier CSV contenant des données de fruits avec trois colonnes (fruit, couleur et prix), nous pouvons le lire de la manière suivante :

```elixir
data = File.read!("fruits.csv")
CSV.parse(data, headers: true)
```

Le paramètre `headers: true` indique que la première ligne du fichier est une ligne d'en-tête contenant les noms des colonnes. Cela nous permet d'accéder aux données de chaque ligne en utilisant ces noms de colonne, par exemple `line["fruit"]` pour accéder au nom du fruit dans la ligne. 

Nous pouvons également utiliser la fonction `CSV.encode/1` pour convertir une liste de lignes en un fichier CSV. Par exemple, si nous avons une liste de tuples représentant des données de fruits, nous pouvons les encoder et les écrire dans un fichier CSV de la manière suivante :

```elixir
fruits = [ {"Apple", "Red", "2.99"}, {"Banana", "Yellow", "1.50"} ]
data = CSV.encode(fruits, headers: [:fruit, :color, :price])
File.write!("fruits.csv", data)
```

Cela produira un fichier CSV avec les mêmes données que nous avons lues précédemment. 

Pour plus d'informations sur les fonctions disponibles pour travailler avec des CSV en utilisant Elixir, vous pouvez consulter la documentation de `CSV` en exécutant `h CSV` dans votre terminal.

## Approfondissement

Il existe plusieurs options disponibles pour personnaliser la façon dont les données sont lues et écrites lors de l'utilisation de `CSV`. Par exemple, nous pouvons utiliser des options pour spécifier un délimiteur de colonne différent de la virgule par défaut, ou pour définir des convertisseurs personnalisés pour gérer des types de données spécifiques. 

De plus, si nous avons un fichier CSV volumineux et que nous ne voulons pas charger toutes les lignes en mémoire à la fois, nous pouvons utiliser la fonction `CSV.stream/2` pour lire et traiter les données ligne par ligne. Cette fonction renvoie un flux qui peut ensuite être manipulé à l'aide de fonctions de flux telles que `Enum.map/2` ou `Stream.filter/2`.

## Voir aussi

- Documentation officielle de CSV : https://hexdocs.pm/csv/CSV.html
- Tutoriel sur le traitement des CSV en utilisant Elixir : https://kipalog.com/posts/Elixir-va-Xu-ly-tap-tin-CSV