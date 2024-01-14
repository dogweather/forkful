---
title:                "Elixir: Conversion d'une date en chaîne de caractères"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Convertissez une date en chaîne de caractères peut sembler un concept simple mais c'est en fait une tâche importante en programmation. La conversion de données en différents formats est courante dans les applications et la manipulation de dates est un élément essentiel dans de nombreux programmes. Dans cet article, nous allons découvrir comment convertir une date en chaîne de caractères en utilisant le langage de programmation Elixir.

## Comment faire

Pour convertir une date en chaîne de caractères en Elixir, nous pouvons utiliser la fonction `~D`, qui accepte une date dans le format ISO et renvoie une chaîne de caractères. Voici un exemple:

```Elixir
~D[2020-12-25]
```

Cela renverra la chaîne de caractères "2020-12-25". Nous pouvons également ajouter un format d'affichage personnalisé en utilisant la fonction `~D[f <<format>>]`. Voici un exemple avec le format "MM/DD/YY":

```Elixir
~D[f "MM/DD/YY"][2020-12-25]
```

Cela renverra la chaîne de caractères "12/25/20". Nous pouvons également formater les dates avec l'heure en utilisant la fonction `~N`, qui suit la même logique. Voici un exemple:

```Elixir
~N[f "MM/DD/YY hh:mm:ss"][2020-12-25 23:59:59]
```

Cela renverra la chaîne de caractères "12/25/20 23:59:59".

## Plongée profonde

En utilisant la fonction `~D[f <<format>>]`, nous pouvons y inclure des options pour la langue et les fuseaux horaires. Cela peut être utile lors de la conversion de dates pour différentes régions ou cultures. De plus, il est important de noter que lorsque nous convertissons des dates en chaînes de caractères, nous perdons les informations sur les fuseaux horaires. Si nous avons besoin de garder cette information, nous pouvons utiliser la bibliothèque `Tzdata` pour travailler avec les fuseaux horaires en Elixir.

## Voir aussi

- [Documentation officielle d'Elixir sur les chaînes de caractères](https://elixir-lang.org/getting-started/strings-and-binaries.html)
- [Documentation officielle d'Elixir sur les dates](https://hexdocs.pm/elixir/DateTime.html)
- [Documentation officielle d'Elixir sur la bibliothèque Tzdata](https://hexdocs.pm/tzdata/readme.html)