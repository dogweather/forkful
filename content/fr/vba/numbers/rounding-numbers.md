---
aliases:
- /fr/vba/rounding-numbers/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:06.082414-07:00
description: "Arrondir les nombres en programmation consiste \xE0 approximer un nombre\
  \ \xE0 son entier le plus proche ou \xE0 un certain nombre de d\xE9cimales. Les\
  \ programmeurs\u2026"
lastmod: 2024-02-18 23:09:08.572216
model: gpt-4-0125-preview
summary: "Arrondir les nombres en programmation consiste \xE0 approximer un nombre\
  \ \xE0 son entier le plus proche ou \xE0 un certain nombre de d\xE9cimales. Les\
  \ programmeurs\u2026"
title: Arrondissement des nombres
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Arrondir les nombres en programmation consiste à approximer un nombre à son entier le plus proche ou à un certain nombre de décimales. Les programmeurs arrondissent les chiffres pour simplifier les figures, améliorer la lisibilité ou répondre à des critères numériques spécifiques dans les calculs, surtout dans les calculs financiers où la précision est importante.

## Comment :

Dans Visual Basic pour Applications (VBA), l'arrondi peut être réalisé en utilisant plusieurs fonctions, chacune étant adaptée à des scénarios spécifiques. Voici les fonctions les plus couramment utilisées avec des exemples :

1. **Fonction Round** :
   La fonction `Round` arrondit un nombre à un nombre spécifié de chiffres.
   ```basic
   Dim roundedNumber As Double
   roundedNumber = Round(3.14159, 2)  ' Résultat : 3.14
   MsgBox roundedNumber
   ```
   
2. **Fonctions Int et Fix** :
   Les fonctions `Int` et `Fix` sont utilisées pour arrondir les nombres à l'entier le plus proche vers le bas, mais elles se comportent différemment avec les nombres négatifs.
   ```basic
   Dim intRounded As Integer
   Dim fixRounded As Integer
   
   intRounded = Int(-3.14159)  ' Résultat : -4
   fixRounded = Fix(-3.14159)  ' Résultat : -3
   
   MsgBox "Int: " & intRounded & ", Fix: " & fixRounded
   ```

3. **Fonctions Ceiling et Floor** :
   VBA n'a pas de fonctions `Ceiling` et `Floor` intégrées trouvées dans d'autres langages. Pour simuler cela, utilisez `Application.WorksheetFunction.Ceiling_Math` et `Application.WorksheetFunction.Floor_Math` pour Excel VBA.
   ```basic
   Dim ceilingNumber As Double
   Dim floorNumber As Double
   
   ceilingNumber = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' Résultat : 4
   floorNumber = Application.WorksheetFunction.Floor_Math(3.14159)  ' Résultat : 3
   
   MsgBox "Ceiling: " & ceilingNumber & ", Floor: " & floorNumber
   ```

## Approfondissement

La fonction `Round` dans VBA est intrinsèquement différente des méthodes d'arrondi dans d'autres langages en raison de son utilisation de l'**arrondi du banquier**. L'arrondi du banquier arrondit au nombre pair le plus proche lorsque exactement à mi-chemin entre deux nombres, réduisant le biais dans les calculs sur un grand ensemble de données et fournissant un résultat statistiquement plus significatif. Cependant, cela peut conduire à un comportement inattendu pour ceux qui ne sont pas familiers avec cela, surtout lorsque une précision intégrale est attendue dans chaque cas.

En contraste, de nombreux langages de programmation et systèmes utilisent "l'arrondi arithmétique" ou "l'arrondi à la moitié supérieure", où un nombre exactement à mi-chemin entre deux valeurs arrondies possibles est toujours arrondi vers le haut. Lors de la traduction ou du portage de code d'autres langues vers VBA, les programmeurs doivent garder ces différences à l'esprit pour éviter les bugs subtils ou les inexactitudes dans les applications financières et statistiques.

Bien que VBA offre une variété de fonctions pour l'arrondi, l'absence de fonctions `Ceiling` et `Floor` (sans recourir à la WorksheetFunction d'Excel) met en évidence une limitation dans ses capacités natives. Les programmeurs provenant de langages plus riches en fonctionnalités peuvent trouver ces omissions gênantes et pourraient avoir besoin de mettre en œuvre des solutions personnalisées ou d'adapter leurs calculs pour utiliser les fonctions disponibles. Malgré ces limitations, comprendre et utiliser correctement les fonctions d'arrondi de VBA peut aider à garantir que les calculs numériques sont à la fois précis et répondent aux exigences de la plupart des applications.
