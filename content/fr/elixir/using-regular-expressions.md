---
title:                "Elixir: Utiliser des expressions régulières"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

"Pourquoi : Utiliser des expressions régulières en programmation Elixir

Les expressions régulières sont un outil puissant pour manipuler et traiter des données de manière efficace. En utilisant des expressions régulières en Elixir, vous pouvez rechercher, filtrer et modifier rapidement des chaînes de caractères selon des motifs spécifiques. Cela peut s'avérer très utile lors de la validation de données entrées par l'utilisateur, du traitement de fichiers ou de la mise en forme de données pour une sortie spécifique. Les expressions régulières peuvent également vous faire gagner du temps en automatisant des tâches fastidieuses liées à la manipulation de chaînes de caractères.

Comment Faire :

Pour utiliser des expressions régulières en Elixir, vous pouvez utiliser la fonction `Regex.match?` ou `Regex.run`. Par exemple, si vous avez une chaîne de caractères contenant des numéros de téléphone au format international, vous pouvez utiliser une expression régulière pour extraire les numéros de téléphone à partir de cette chaîne.

```Elixir
texte = "Mon numéro de téléphone est +33 6 12 34 56 78."
Regex.run(~r/[+]\d{2} \d{1} \d{2} \d{2} \d{2} \d{2}/, texte)

# Output: [+33 6 12 34 56 78]
```

Vous pouvez également utiliser des expressions régulières pour effectuer des remplacements de caractères dans une chaîne, par exemple pour supprimer tous les espaces dans un texte.

```Elixir
texte = "Bonjour tout le monde!"
Regex.replace(~r/\s/, texte, "")

# Output: "Bonjourtoutlemonde!"
```

En utilisant des caractères spéciaux et des quantificateurs, vous pouvez créer des motifs de recherche plus complexes. Vous pouvez également utiliser des groupes de capture pour récupérer des parties spécifiques d'une chaîne correspondant au motif.

En fin de compte, l'utilisation de régulières peut paraître compliquée au début, mais une fois que vous vous familiarisez avec les différents caractères et quantificateurs, cela devient un outil extrêmement puissant pour manipuler vos chaînes de caractères en Elixir.

Plongée dans les détails :

Si vous souhaitez en savoir plus sur les expressions régulières, il existe de nombreuses ressources en ligne pour approfondir vos connaissances. Vous pouvez également consulter la documentation officielle d'Elixir qui donne un aperçu détaillé des différents caractères et quantificateurs disponibles.

Voir aussi :

- [Documentation officielle d'Elixir sur les expressions régulières](https://hexdocs.pm/elixir/Regex.html)
- [Site web de regex101 pour tester et expérimenter avec des expressions régulières](https://regex101.com/)
- [Tutoriel d'expressions régulières d'OpenClassrooms (en français)](https://openclassrooms.com/fr/courses/918836-concevez-votre-site-web-avec-php-et-mysql/913820-les-expressions-regulieres-1)