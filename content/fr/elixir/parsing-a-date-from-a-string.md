---
title:                "Extraire une date d'une chaîne de caractères"
html_title:           "Elixir: Extraire une date d'une chaîne de caractères"
simple_title:         "Extraire une date d'une chaîne de caractères"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Elixir: Analyser des dates à partir de chaînes de caractères

## Quoi et pourquoi?

Analyser une date à partir d'une chaîne de caractères, c'est extraire la date et l'heure contenues dans une chaîne pour les transformer en un format utilisable par le programme. Les programmeurs utilisent cette fonctionnalité pour valider les entrées de l'utilisateur et manipuler les dates dans leurs applications.

## Comment faire:

```Elixir
iex> Date.from_iso8601("2021-07-15")
{:ok, ~D[2021-07-15]}
```

L'exemple ci-dessus utilise la fonction `from_iso8601` pour extraire et formater la date contenue dans la chaîne `"2021-07-15"`. Avec le marquage de date (`~D`), nous obtenons une date standard que nous pouvons utiliser dans notre programme.

```Elixir
iex> DateTime.from_iso8601("2021-07-15T09:30:00")
{:ok, ~U[2021-07-15T09:30:00Z]}
```

De même, la fonction `from_iso8601` peut également extraire l'heure en plus de la date. Avec le marquage de temps (`~U`), nous obtenons un objet `DateTime` pour manipuler l'heure dans notre programme.

Ces fonctions sont très utiles pour valider les entrées de l'utilisateur pour s'assurer qu'elles correspondent à un format spécifique de date et heure.

## Plongez-y:

### Contexte historique:

L'analyse de dates à partir de chaînes de caractères est une fonctionnalité qui est devenue de plus en plus importante avec l'utilisation croissante d'Internet et des formats de date et heure normalisés. Cette fonctionnalité est disponible dans la plupart des langages de programmation aujourd'hui, y compris Elixir.

### Alternatives:

Outre les fonctions `from_iso8601` présentées dans cet article, il existe d'autres fonctions Elixir telles que `from_naive_datetime` et `from_unix` pour analyser des dates à partir de chaînes de caractères selon différents formats.

### Détails de mise en œuvre:

En interne, ces fonctions utilisent le module `Calendar` dans la librairie standard d'Elixir pour effectuer la conversion de la date et heure en un format utilisable par le programme.

## Voir aussi:

- [Documentation sur les dates dans Elixir](https://hexdocs.pm/elixir/Calendar.html)
- [Aperçu des formats de date et heure dans Elixir](https://elixir-lang.org/getting-started/basic-types.html#dates-times-and-time-zones)