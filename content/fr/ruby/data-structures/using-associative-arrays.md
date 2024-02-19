---
aliases:
- /fr/ruby/using-associative-arrays/
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:54.615524-07:00
description: "Les tableaux associatifs, plus commun\xE9ment appel\xE9s hash en Ruby,\
  \ permettent d'associer des cl\xE9s uniques \xE0 des valeurs. Ils sont indispensables\
  \ lorsque\u2026"
lastmod: 2024-02-18 23:09:09.397215
model: gpt-4-0125-preview
summary: "Les tableaux associatifs, plus commun\xE9ment appel\xE9s hash en Ruby, permettent\
  \ d'associer des cl\xE9s uniques \xE0 des valeurs. Ils sont indispensables lorsque\u2026"
title: Utilisation des tableaux associatifs
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Les tableaux associatifs, plus communément appelés hash en Ruby, permettent d'associer des clés uniques à des valeurs. Ils sont indispensables lorsque vous avez besoin de suivre des éléments à travers une référence spécifique, comme stocker les propriétés d'un objet ou accéder rapidement à des données par un identifiant unique.

## Comment faire :

Créer et utiliser des hash en Ruby est simple. Vous pouvez initialiser un hash vide, le remplir avec des paires clé-valeur, accéder aux valeurs par leurs clés, et plus. Voici comment vous procédez :

```Ruby
# Création d'un hash
my_hash = { "name" => "John Doe", "age" => 30 }

# Une autre façon de créer un hash
another_hash = Hash.new
another_hash["position"] = "Developer"

# Accéder aux valeurs du hash
puts my_hash["name"] # Sortie : John Doe

# Ajouter une nouvelle paire clé-valeur
my_hash["language"] = "Ruby"
puts my_hash # Sortie : {"name"=>"John Doe", "age"=>30, "language"=>"Ruby"}

# Itérer à travers un hash
my_hash.each do |key, value|
  puts "#{key}: #{value}"
end
# Sortie :
# name: John Doe
# age: 30
# language: Ruby
```

Vous pouvez également utiliser des symboles comme clés plus efficaces :

```Ruby
# Utiliser des symboles pour les clés
symbol_hash = { name: "Jane Doe", age: 22 }
puts symbol_hash[:name] # Sortie : Jane Doe
```

## Approfondissement :

Le concept de tableaux associatifs n'est pas unique à Ruby ; de nombreux langages les implémentent sous différents noms, comme les dictionnaires en Python ou les objets en JavaScript (lorsqu'ils sont utilisés comme paires clé-valeur). Aux premiers stades de Ruby, les hash étaient quelque peu plus lents et pas aussi polyvalents. Cependant, avec le temps, l'implémentation des hash par Ruby est devenue très optimisée, surtout pour les clés symboles, les rendant extrêmement efficaces pour un accès et des mises à jour fréquents.

Les hash de Ruby se distinguent par leur facilité d'utilisation syntaxique et leur flexibilité - vous pouvez utiliser presque n'importe quel type d'objet comme clé, bien que les symboles et les chaînes soient les plus courants. En interne, les hash Ruby sont implémentés à l'aide d'un algorithme de hachage qui équilibre la vitesse et l'efficacité de la mémoire, même lorsque le nombre d'éléments augmente.

Bien que les hash soient incroyablement polyvalents, ils ne sont pas la solution ultime pour le stockage de données en Ruby. Pour les collections ordonnées, les tableaux sont plus appropriés, et pour les ensembles d'articles uniques, un Set pourrait être un meilleur choix. De plus, pour des structures de données très complexes, il pourrait être conseillé de créer des classes personnalisées.

Rappelez-vous, le choix d'utiliser un hash plutôt que d'autres structures de données se résume largement au cas d'utilisation spécifique - les hash excellent dans les recherches rapides et le maintien des associations entre des clés uniques et leurs valeurs.
