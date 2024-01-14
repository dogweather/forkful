---
title:                "Elixir: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil puissant pour la manipulation de chaînes de caractères dans un programme Elixir. Elles permettent de rechercher et de remplacer des motifs spécifiques dans une chaîne, ce qui peut être utile pour diverses tâches telles que la validation de données d'utilisateur ou le traitement de fichiers.

## Comment utiliser les expressions régulières en Elixir

Les expressions régulières en Elixir sont déclarées en utilisant la fonction `~r`, suivie du motif que vous souhaitez rechercher entre des guillemets. Voici un exemple de recherche de toutes les lettres majuscules dans une chaîne:

```elixir
~r/[A-Z]/
```

Vous pouvez ensuite utiliser cette expression régulière dans des fonctions telles que `Regex.match?` pour vérifier si une chaîne donnée correspond au motif.

En voici un exemple complet:

```elixir
string = "Bonjour le monde"
pattern = ~r/[A-Z]/
Regex.match?(pattern, string)
# Résultat: false
```

Vous pouvez également utiliser une expression régulière pour remplacer des parties d'une chaîne en utilisant la fonction `Regex.replace`:

```elixir
string = "Le soleil est jaune"
Regex.replace(~r/jaune/, string, "rouge")
# Résultat: "Le soleil est rouge"
```

## Approfondissement dans les expressions régulières

Les expressions régulières en Elixir utilisent la syntaxe PCRE (Perl Compatible Regular Expressions) et offrent des options avancées telles que l'utilisation de groupes de capture et de quantificateurs. Il existe également des fonctions spéciales pour rechercher des motifs spécifiques tels que les numéros de téléphone ou les adresses e-mail.

Il est important de noter que les expressions régulières peuvent être difficiles à lire et à comprendre, il est donc essentiel de bien documenter votre code et de le tester soigneusement.

## Voir aussi

- [Documentation Elixir sur les expressions régulières](https://hexdocs.pm/elixir/Regex.html)
- [Tutoriel Elixir sur les expressions régulières](https://elixirschool.com/fr/lessons/advanced/regex/)
- [Site web pour tester les expressions régulières](https://regex101.com/)