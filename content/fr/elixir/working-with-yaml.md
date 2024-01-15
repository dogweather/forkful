---
title:                "Travailler avec yaml"
html_title:           "Elixir: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données structurées, telles que des configurations de programme, des données de test ou des données de configuration, alors vous avez probablement déjà entendu parler de YAML (YAML Ain't Markup Language). YAML est un format de sérialisation de données facile à lire et à écrire pour les humains, qui peut être utilisé avec de nombreux langages de programmation. Dans cet article, nous allons expliquer pourquoi YAML est un excellent choix pour travailler avec des données structurées et comment l'utiliser efficacement en utilisant Elixir.

## Comment Faire

Pour commencer à travailler avec YAML en utilisant Elixir, vous devez d'abord installer le package `YamlElixir` en utilisant Mix:

```Elixir
mix deps.get YamlElixir
```

Une fois le package installé, vous pouvez l'utiliser dans votre code en l'important en tant que module:

```Elixir
import YamlElixir
```

Ensuite, vous pouvez utiliser la fonction `YamlElixir.load/1` pour charger des données YAML à partir d'une chaîne ou d'un fichier:

```Elixir
yaml_string = "---\ntitle: 'Nouvel article'\nauthor: 'Jeanne'\ncontent: 'Contenu de l'article'\n"
data = YamlElixir.load(yaml_string)

# output: %{author: "Jeanne", content: "Contenu de l'article", title: "Nouvel article"}
```

Vous pouvez également utiliser la fonction `YamlElixir.dump/1` pour sérialiser des données en YAML:

```Elixir
data = %{title: "Nouvel article", author: "Jeanne", content: "Contenu de l'article"}
yaml_string = YamlElixir.dump(data)

# output: "---\ntitle: 'Nouvel article'\nauthor: 'Jeanne'\ncontent: 'Contenu de l'article'\n"
```

## Plongée Profonde

YAML prend en charge plusieurs types de données tels que les chaînes, les tableaux et les objets, ainsi que des fonctionnalités telles que les balises personnalisées et les ancres pour référencer des données. Pour en savoir plus sur ces fonctionnalités, vous pouvez consulter la documentation officielle de YAML [ici](https://yaml.org/spec/1.2/spec.html).

De plus, le package YamlElixir offre des options pour personnaliser le traitement des données YAML, telles que la prise en charge de données binaires et la conversion de nombres en entiers. Vous pouvez en savoir plus sur ces options dans la documentation du package [ici](https://hexdocs.pm/yaml_elixir/YamlElixir.html#module-default-options).

## Voir Aussi

- [Documentation officielle de YAML](https://yaml.org/spec/1.2/spec.html)
- [Documentation de YamlElixir](https://hexdocs.pm/yaml_elixir/YamlElixir.html)