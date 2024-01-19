---
title:                "Supprimer les caractères correspondant à un modèle"
html_title:           "Ruby: Supprimer les caractères correspondant à un modèle"
simple_title:         "Supprimer les caractères correspondant à un modèle"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

La suppression des caractères correspondant à un motif est un processus qui retire tous les caractères d'une chaîne qui correspondent à un modèle spécifique. Les programmeurs l'utilisent souvent pour nettoyer ou normaliser les données textuelles.

## Comment faire :

Voici comment vous pouvez supprimer des caractères dans Ruby en utilisant la méthode 'delete'. Supposons que nous voulions supprimer toutes les voyelles d'une chaîne.

```Ruby
chaine = "Bonjour, comment ça va ?"
nouvelle_chaine = chaine.delete 'aeiouyAEIOUY'
puts nouvelle_chaine
```

Et voici la sortie de ce code :

```
Bnjr, cmmnt ç v ?
```

## Plongée en profondeur

La méthode 'delete' existe en Ruby depuis ses débuts, ce qui démontre sa nécessité dans les opérations sur les chaînes. En termes d'alternatives, vous pouvez utiliser une expression régulière avec la méthode 'gsub' pour atteindre le même objectif, mais 'delete' est généralement plus performant pour des opérations simples de suppression de caractères.

```Ruby
chaine = "Bonjour, comment ça va ?"
nouvelle_chaine = chaine.gsub(/[aeiouyAEIOUY]/, '')
puts nouvelle_chaine
```

L'implémentation interne de 'delete' est assez simple en Ruby. La méthode itère chaque caractère de la chaîne et vérifie si ce caractère est dans la chaîne de caractères à supprimer. Si c'est le cas, il est supprimé.

## Voir aussi

Pour plus d'informations sur la manipulation de chaînes en Ruby, consultez les sources suivantes :

- La documentation officielle de Ruby sur les chaînes : [https://ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)
- Un guide pratique pour gérer les expressions régulières en Ruby : [https://www.rubyguides.com/2015/06/ruby-regex/](https://www.rubyguides.com/2015/06/ruby-regex/)