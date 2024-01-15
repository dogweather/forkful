---
title:                "Travailler avec yaml"
html_title:           "PHP: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes programmeur PHP, vous avez probablement déjà entendu parler de YAML. Mais savez-vous pourquoi vous devriez l'utiliser dans vos projets ? Eh bien, le YAML est un langage de configuration flexible et facile à lire, qui peut grandement faciliter la gestion de données dans vos applications. Dans cet article, nous allons plonger dans le monde du YAML et découvrir pourquoi c'est un outil précieux pour tout programmeur PHP.

## Comment faire

Pour commencer à utiliser YAML dans votre code PHP, vous devez inclure la librairie Symfony YAML dans votre projet. Vous pouvez le faire en utilisant Composer, en exécutant la commande suivante dans votre terminal : `composer require symfony/yaml`.

Une fois que la librairie est installée, vous pouvez utiliser la classe `Yaml` pour lire et écrire des données au format YAML. Voici un exemple de code qui lit un fichier YAML et affiche son contenu :

```PHP
use Symfony\Component\Yaml\Yaml;

$file = "config.yml";
$config = Yaml::parse(file_get_contents($file));
print_r($config);
```

Et voici le contenu du fichier `config.yml` :

```yaml
# config.yml
database:
  host: localhost
  user: root
  password: 1234
```

Lorsque vous exécutez le code, vous devriez obtenir la sortie suivante :

```
Array
(
    [database] => Array
        (
            [host] => localhost
            [user] => root
            [password] => 1234
        )

)
```

Vous pouvez également écrire des données au format YAML en utilisant la fonction `dump` :

```PHP
use Symfony\Component\Yaml\Yaml;

$config = [
    "database" => [
        "host" => "localhost",
        "user" => "root",
        "password" => "1234"
    ]
];

echo Yaml::dump($config);
```

Et voici la sortie :

```yaml
database:
  host: localhost
  user: root
  password: 1234
```

Vous pouvez également utiliser des tableaux multidimensionnels pour représenter des données complexes dans votre fichier YAML, comme des listes ou des tableaux associatifs. Voici un exemple de code pour écrire et lire un fichier YAML avec des données plus complexes :

```PHP
use Symfony\Component\Yaml\Yaml;

$movies = [
    [
        "title" => "Inception",
        "year" => 2010,
        "director" => "Christopher Nolan",
        "actors" => ["Leonardo DiCaprio", "Joseph Gordon-Levitt", "Ellen Page"]
    ],
    [
        "title" => "The Shawshank Redemption",
        "year" => 1994,
        "director" => "Frank Darabont",
        "actors" => ["Tim Robbins", "Morgan Freeman"]
    ]
];

// Écrire les données dans un fichier YAML
Yaml::dump($movies, $file);

// Lire le fichier YAML et le convertir en tableau associatif
$movies = Yaml::parse(file_get_contents($file));

// Parcourir le tableau et afficher les données
foreach ($movies as $movie) {
    echo $movie["title"] . " (" . $movie["year"] . ") directed by " . $movie["director"] . " and starring ";
    echo implode(", ", $movie["actors"]) . PHP_EOL;
}
```

Et voici la sortie :

```
Inception (2010) directed by Christopher Nolan and starring Leonardo DiCaprio, Joseph Gordon-Levitt, Ellen Page
The Shawshank Redemption (1994) directed by Frank Darabont and starring Tim Robbins, Morgan Freeman
```

## Plongée en profondeur

Maintenant que vous savez comment lire et écrire des données au format YAML en utilisant la librairie Symfony, voyons quelques astuces pour travailler avec YAML de manière plus efficace.

Tout d'abord, vous pouvez personnaliser la façon dont les données sont écrites en utilisant plusieurs options dans la fonction `dump`. Par exemple, vous pouvez choisir d'utiliser des indentations ou des tabulations pour formater votre fichier YAML, ou même activer la prise en charge des références pour éviter les duplications de données.

Deuxièmement, vous pouvez utiliser des commentaires dans votre fichier YAML en les ajoutant après le symbole `#`. Cela peut être utile pour documenter vos données ou ajouter des notes pour vous-même ou pour d'autres membres de votre équipe.

De plus, la libra