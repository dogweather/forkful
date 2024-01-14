---
title:    "Elixir: Supprimer les caractères correspondant à un motif"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi

Supprimer ou enlever des caractères qui correspondent à un certain motif est une tâche courante en programmation. Que ce soit pour nettoyer des données, valider des entrées utilisateur ou manipuler des chaînes de caractères, la suppression de caractères basée sur un motif est souvent utilisée pour transformer ou filtrer des données. Dans cet article, nous allons explorer comment cela peut être fait en utilisant le langage de programmation Elixir.

## Comment faire

Pour supprimer des caractères basés sur un motif en Elixir, nous pouvons utiliser la fonction `Regex.replace/3` qui prend en entrée une expression régulière et remplace tous les correspondances par une chaîne de caractères spécifiée. Par exemple, pour supprimer tous les signes de ponctuation dans une chaîne de caractères, nous pouvons écrire :

```Elixir
input = "Salut ! Comment vas-tu ?"
Regex.replace(~r/[[:punct:]]/, input, "")
```

Cela va retourner `Salut Comment vas-tu` en supprimant tous les signes de ponctuation de la chaîne de caractères.

Nous pouvons également utiliser la même fonction pour effectuer des remplacements plus avancés en utilisant des groupes de capture dans l'expression régulière. Par exemple, si nous voulons supprimer tous les nombres d'une chaîne de caractères, nous pouvons utiliser l'expression régulière `~r/\d+/` qui va correspondre à tous les nombres dans la chaîne de caractères. En combinant cela avec des groupes de capture et en utilisant le caractère de substitution `\1`, nous pouvons présenter les nombres correspondants tels quels sans avoir à les remplacer par une chaîne vide :

```Elixir
input = "Il y avait 5 pommes dans le panier."
Regex.replace(~r/(\d+)/, input, "\1")
```

Cela va retourner `Il y avait 5 pommes dans le panier` en gardant les nombres dans la chaîne de caractères.

## Plongée en profondeur

En Elixir, les expressions régulières sont représentées par des structures de données binaires appelées "binaires de bits" afin de les rendre plus efficaces et plus rapides à traiter. Cela signifie que les expressions régulières dans Elixir fonctionnent de manière légèrement différente des expressions régulières dans d'autres langages, et qu'il peut y avoir des différences de performances lors de l'utilisation de certaines fonctionnalités avancées comme les groupes de capture.

Il est également important de noter que la suppression de caractères basée sur des expressions régulières n'est pas la seule façon de le faire en Elixir. Il existe d'autres méthodes comme l'utilisation de fonctions de manipulation de chaînes de caractères ou même de listes de caractères pour atteindre le même objectif. Dans certains cas, ces méthodes peuvent être plus performantes et plus faciles à comprendre par rapport à l'utilisation d'expressions régulières.

## Voir aussi

- [Documentation officielle d'Elixir sur Regex](https://hexdocs.pm/elixir/Regex.html)
- [Tutoriel sur les expressions régulières en Elixir](https://dev.to/eevajonnapanula/using-regex-in-elixir-2b6m)
- [Article sur la manipulation de chaînes de caractères en Elixir](https://medium.com/@jtmthf/elixir-st