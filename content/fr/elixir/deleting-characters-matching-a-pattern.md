---
title:                "Elixir: Suppression de caractères correspondants à un motif"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous travaillez avec du texte, il peut arriver que vous souhaitiez supprimer des caractères qui correspondent à un certain motif. Cela peut être utile si vous souhaitez nettoyer une chaîne de caractères ou extraire certaines informations spécifiques.

# Comment faire

Dans Elixir, vous pouvez facilement supprimer des caractères correspondant à un motif en utilisant la méthode `String.replace/4`. Cette méthode prend en paramètre une chaîne de caractères, le motif à rechercher, la chaîne de remplacement et des options facultatives.

```
iex> String.replace("Bonjour!", ~r/o+/, "-")
"B-nj-r!"
```

Dans cet exemple, nous remplaçons toutes les occurrences de la lettre "o" par un tiret "-" dans la chaîne "Bonjour!". Nous utilisons également une expression régulière pour indiquer que nous voulons rechercher toutes les occurrences de la lettre "o" en tant que motif.

# Plongée profonde

Il est important de noter que la méthode `String.replace/4` ne modifie pas la chaîne d'origine, elle renvoie plutôt une nouvelle chaîne avec les modifications apportées. Si vous voulez modifier la chaîne d'origine, vous pouvez utiliser la méthode `String.replace!/4` en ajoutant le symbole "!" à la fin. Cela rendra la méthode destructive et modifie la chaîne d'origine.

Lorsque vous utilisez cette méthode, vous pouvez également utiliser des groupes de capture dans votre motif et les utiliser dans la chaîne de remplacement. Par exemple :

```
iex> String.replace("12/10/2021", ~r/(\d{2})\/(\d{2})\/(\d{4})/, "$2-$1-$3")
"10-12-2021"
```

Ici, nous utilisons des groupes de capture pour extraire le jour, le mois et l'année de la chaîne "12/10/2021" et les réorganiser dans un nouveau format.

# Voir aussi

- [Documentation officielle d'Elixir pour String.replace/4](https://hexdocs.pm/elixir/String.html#replace/4)
- [Liste des expressions régulières en Elixir](https://medium.com/@JottedPictionary/a-beginners-guide-to-regular-expressions-regex-in-elixir-5ca9c6f34184)
- [Différence entre les méthodes destructives et non destructives en Elixir](https://elixir-lang.org/getting-started/runtime-errors.html#destructive-functions)