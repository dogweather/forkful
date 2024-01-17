---
title:                "Utiliser les expressions régulières"
html_title:           "Fish Shell: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs l'utilisent-ils?

Les expressions régulières sont des modèles de caractères utilisés pour trouver des motifs spécifiques dans du texte. Les programmeurs utilisent les expressions régulières pour automatiser la recherche et le traitement de données dans des fichiers texte, ce qui peut leur faire gagner beaucoup de temps et d'efforts.

## Comment faire:

Voici quelques exemples de code utilisant les expressions régulières dans le Fish Shell avec leur résultat :

```
# Trouver toutes les lignes contenant un mot qui commence par "p"
grep -E "p\w+" text.txt
# Résultat : fish, pour, partir, passe

# Remplacer tous les mots qui commencent par "h" par "ville" dans le fichier text.txt
sed -E "s/h\w+/ville/g" text.txt
# Résultat : Je suis allé à la ville, la ville était belle, j'ai quitté la ville

# Vérifier si une chaîne de caractères correspond à un schéma d'adresse email
if string match -r "^\w+@\w+\.\w+$" $email
  echo "Valide"
else
  echo "Non valide"
end
```

## Plongée en profondeur:

Les expressions régulières existent depuis les années 1950 et ont été développées pour être utilisées dans les langages de programmation. Bien que le Fish Shell ait son propre ensemble de commandes pour travailler avec les expressions régulières, d'autres langages de script comme Perl, Python et Java ont également des bibliothèques dédiées à leur utilisation.

Les alternatives aux expressions régulières sont les fonctions de manipulation de texte intégrées dans la plupart des langages de programmation. Cependant, ces fonctions peuvent être limitées et moins flexibles que les expressions régulières.

L'implémentation des expressions régulières dans le Fish Shell est basée sur la bibliothèque "PCRE" (Perl Compatible Regular Expressions), qui fournit des fonctionnalités avancées pour manipuler des modèles de caractères complexes.

## Voir aussi:

Pour en savoir plus sur les expressions régulières et leur utilisation dans le Fish Shell, vous pouvez consulter les ressources suivantes :

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutoriel pour débuter avec les expressions régulières dans le Fish Shell](https://fishshell.com/docs/current/index.html#quick-tutorial-on-fishs-easy-string-substitution-methods)
- [Cheat sheet pour les expressions régulières dans le Fish Shell](https://hackersandslackers.com/scrape-an-array-fish/)