---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi ?

Supprimer des caractères correspondant à un motif est une opération courante en programmation qui consiste à enlever toutes les instances d'un motif spécifique dans une chaîne. Les programmeurs le font souvent pour nettoyer ou transformer des données.

## Comment faire :

En Elixir, nous utilisons la fonction `String.replace/3` pour supprimer un motif spécifique :

```elixir
defmodule MonModule do
  def supprimer_caracteres(motif, chaine) do
    String.replace(chaine, motif, "")
  end
end

IO.puts MonModule.supprimer_caracteres("a", "banane") # => "bnne"
```
Dans cet exemple, la fonction `supprimer_caracteres` remplace toutes les instances du motif (ici "a") par une chaîne vide, donc supprime les "a".

## Plongée profonde :

Historiquement, supprimer des caractères d'une chaîne en utilisant un motif était plus complexe. Les langages de programmation comme Elixir ont rendu cette opération plus simple et plus intuitive.

Côté performance, `String.replace/3` scanne toute la chaîne, ce qui peut être coûteux en temps pour des chaînes énormes. Si la performance est un problème, cherchez des alternatives comme travailler avec des listes de caractères.

En Elixir, `String.replace/3` utilise un algorithme de recherche de motif efficace. Cependant, il n'est pas conçu pour supprimer un grand nombre de motifs différents en même temps.

## Voir Aussi :

Pour plus d'informations sur le traitement des chaînes en Elixir, consultez les documents Elixir sur les chaînes : https://hexdocs.pm/elixir/String.html

Pour une discussion sur les motifs en programmation, je recommande cet article : https://en.wikipedia.org/wiki/Pattern_matching

Pour explorer plus en détail la performance de l'opération de remplacement de chaîne, ce lien peut être utile : https://stackoverflow.com/questions/57677767/elixir-string-replace-performance