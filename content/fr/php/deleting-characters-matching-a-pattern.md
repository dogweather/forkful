---
title:                "Suppression de caractères correspondant à un modèle."
html_title:           "PHP: Suppression de caractères correspondant à un modèle."
simple_title:         "Suppression de caractères correspondant à un modèle."
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut être nécessaire de supprimer des caractères correspondant à un motif dans un programme PHP pour diverses raisons, telles que nettoyer des données entrantes ou formater une chaîne de caractères pour un affichage spécifique. En comprenant comment supprimer efficacement ces caractères, vous pourrez améliorer la qualité et la fiabilité de votre code.

## Comment faire

Pour supprimer des caractères correspondant à un motif dans une chaîne en PHP, vous pouvez utiliser la fonction native `preg_replace()`. Cette fonction accepte trois arguments : le motif, la chaîne de caractères à vérifier et la chaîne de remplacement. Voici un exemple de code qui supprime tous les chiffres d'une chaîne de caractères et affiche la nouvelle chaîne de caractères :

```PHP
$motif = '/[0-9]/';
$chaine = "Hello123";
$chaine_modifiee = preg_replace($motif, "", $chaine);
echo $chaine_modifiee; // Affiche "Hello"
```

En utilisant des expressions régulières, vous pouvez également affiner votre motif pour supprimer des caractères spécifiques. Par exemple, si vous souhaitez supprimer uniquement les chiffres après le premier caractère, vous pouvez utiliser le motif `'/[0-9]+/'` qui ne supprimera que les chiffres qui sont suivis d'au moins un autre chiffre.

## Plongée en profondeur

Lorsque vous utilisez la fonction `preg_replace()`, il est important de comprendre la notation des expressions régulières utilisée pour définir le motif. Voici quelques exemples couramment utilisés :

- `[0-9]` : correspond à un seul chiffre
- `[a-z]` : correspond à une seule lettre minuscule
- `.` : correspond à un seul caractère de n'importe quel type
- `+` : correspond à un ou plusieurs occurrences du caractère précédent
- `*` : correspond à zéro ou plusieurs occurrences du caractère précédent
- `\s` : correspond à un espace blanc
- `\d` : correspond à un chiffre de 0 à 9
- `\w` : correspond à un caractère alphanumérique (lettre, chiffre ou souligné)

Pour une liste complète des expressions régulières en PHP, vous pouvez consulter la documentation officielle [ici](https://www.php.net/manual/fr/regexp.reference.escape.php).

## Voir aussi

- [Documentation officielle de la fonction `preg_replace()`](https://www.php.net/manual/fr/function.preg-replace.php)
- [Tutoriel sur les expressions régulières en PHP](https://www.tutorialspoint.com/php/php_regular_expression.htm)
- [Site pour tester vos expressions régulières en temps réel](https://regex101.com/)