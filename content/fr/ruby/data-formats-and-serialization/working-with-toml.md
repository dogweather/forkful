---
title:                "Travailler avec TOML"
aliases:
- /fr/ruby/working-with-toml.md
date:                  2024-01-26T04:25:36.599550-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/working-with-toml.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

TOML est un format de fichier de configuration qui est facile à lire grâce à sa sémantique claire. Les programmeurs utilisent TOML pour gérer les configurations d'applications et la sérialisation des données sans le poids de XML ou les particularités de YAML.

## Comment faire :

Premièrement, installez le gem `toml-rb`. C'est un choix populaire pour l'analyse TOML en Ruby.

```Ruby
gem install toml-rb
```

Ensuite, pour lire un fichier TOML :

```Ruby
require 'toml-rb'

contenu_toml = File.read('config.toml')
config = TomlRB.parse(contenu_toml)
puts config['title']
```

Un exemple de sortie pourrait être :

```
Mon Application Géniale
```

Pour écrire dans un fichier TOML :

```Ruby
require 'toml-rb'

config = {
  'title' => 'Mon Application Géniale',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

string_toml = TomlRB.dump(config)
File.write('config.toml', string_toml)
```

Vérifiez `config.toml` et vous verrez vos paramètres, soigneusement stockés.

## Plongée Profonde

TOML, qui signifie Tom's Obvious, Minimal Language, a été créé par Tom Preston-Werner, le co-fondateur de GitHub, autour de 2013. Son objectif principal est d’être un format simple qui est facile à analyser en structures de données. Alors que JSON est excellent pour les API, et YAML est flexible, la niche de TOML est son accent sur le fait d'être convivial pour les humains. Contrairement à YAML, qui peut être délicat avec l'indentation, TOML vise une structure plus semblable à INI que beaucoup trouvent plus simple et moins sujette aux erreurs.

Des alternatives comme JSON, YAML ou XML ont chacune leurs propres forces, mais TOML prospère dans des scénarios où une configuration devrait être facilement maintenue par les humains et les programmes de la même façon. Il n'est pas seulement plus simple mais impose un formatage strict et lisible.

Sur le côté technique, pour analyser le contenu TOML avec Ruby, nous exploitons des gems comme `toml-rb`. Ce gem tire parti de la nature dynamique de Ruby, convertissant les données TOML en tableaux associatifs, tableaux et autres structures de données de base de Ruby. Cette conversion signifie que les développeurs peuvent travailler avec des données TOML en utilisant la sémantique et les méthodes familières de Ruby.

## Voir Aussi

- Projet et spécification TOML : https://toml.io/fr/
- Le gem `toml-rb` : https://github.com/emancu/toml-rb
- Comparer TOML, YAML et JSON : https://blog.theodo.com/2021/08/compare-yml-toml-json/
