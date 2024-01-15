---
title:                "Utiliser les expressions régulières"
html_title:           "Elixir: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur ou programmeur, vous avez probablement entendu parler des expressions régulières (Regex pour les intimes). Mais pourquoi voudriez-vous utiliser ces expressions complexes et souvent intimidantes ? Et bien, les expressions régulières sont un outil puissant pour trouver, filtrer et manipuler des chaînes de caractères, ce qui en fait un must-have pour tout arsenal de développement.

## Comment faire

La syntaxe de base des expressions régulières en Elixir est similaire à celle des autres langages de programmation, mais a quelques particularités uniques. Jetons un coup d'oeil à quelques exemples pratiques pour mieux comprendre.

```Elixir
#Trouver un numéro de téléphone américain
Regex.scan(~r/\d{3}-\d{3}-\d{4}/, "Mon numéro de téléphone est 123-456-7890")
#Résultat: [["123-456-7890"]]

#Remplacer les voyelles par des tirets
Regex.replace(~r/[aeiou]/, "Elixir", "-")
#Résultat: "-l-x-r"
```

Comme vous pouvez le constater, les expressions régulières utilisent des symboles spéciaux pour représenter des jeux de caractères ou des motifs. Vous pouvez également utiliser des quantificateurs pour spécifier le nombre de fois qu'un caractère ou un ensemble de caractères doit apparaître. Vous pouvez en apprendre davantage sur la syntaxe en consultant les documents officiels d'Elixir.

## Deep Dive

Les expressions régulières peuvent sembler un peu compliquées au début, mais une fois que vous avez compris les bases, vous verrez à quel point elles sont utiles pour gérer les chaînes de caractères complexes. En plus des exemples ci-dessus, vous pouvez également utiliser des expressions régulières en Elixir pour extraire des données d'HTML, valider des adresses email et même pour créer un analyseur lexical.

Pour aller plus loin, vous pouvez également vous familiariser avec des concepts avancés tels que les groupes capturants, les assertions et les expressions régulières récurrentes. Il existe également des sites web et des outils en ligne tels que Regex101 qui vous permettent de tester et de perfectionner vos expressions régulières.

## Voir aussi

- [Documentation officielle d'Elixir pour les expressions régulières](https://hexdocs.pm/elixir/Regex.html)
- [Regex101 - Outil en ligne pour tester vos expressions régulières](https://regex101.com/)
- [Vidéo d'introduction aux expressions régulières en Elixir](https://www.youtube.com/watch?v=9_8CrdAw8-Q)