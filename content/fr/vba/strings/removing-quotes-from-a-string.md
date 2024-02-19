---
aliases:
- /fr/vba/removing-quotes-from-a-string/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:30.660223-07:00
description: "La suppression des guillemets d'une cha\xEEne dans VBA implique de retirer\
  \ les instances de guillemets simples (`'`) ou doubles (`\"`) qui peuvent encapsuler\u2026"
lastmod: 2024-02-18 23:09:08.563737
model: gpt-4-0125-preview
summary: "La suppression des guillemets d'une cha\xEEne dans VBA implique de retirer\
  \ les instances de guillemets simples (`'`) ou doubles (`\"`) qui peuvent encapsuler\u2026"
title: "Supprimer les guillemets d'une cha\xEEne"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La suppression des guillemets d'une chaîne dans VBA implique de retirer les instances de guillemets simples (`'`) ou doubles (`"`) qui peuvent encapsuler ou être intégrés à l'intérieur de la chaîne. Cette opération est essentielle pour l'assainissement des données, en s'assurant que les chaînes sont formatées correctement pour les requêtes de base de données, l'analyse JSON, ou simplement pour des raisons esthétiques ou de cohérence au sein de l'interface d'une application.

## Comment faire :

Dans VBA, il existe plusieurs méthodes pour supprimer les guillemets d'une chaîne. Voici un exemple simple utilisant la fonction `Replace`, qui recherche une sous-chaîne spécifique (dans ce cas, un guillemet) au sein d'une chaîne et la remplace par une autre sous-chaîne (une chaîne vide si on supprime).

```basic
Sub RemoveQuotesExample()
    Dim originalString As String
    originalString = "'Ceci' est une chaîne de ""test""."
    
    ' Supprimer les guillemets simples
    originalString = Replace(originalString, "'", "")
    
    ' Supprimer les guillemets doubles
    originalString = Replace(originalString, Chr(34), "")
    
    Debug.Print originalString 'Sortie : Ceci est une chaîne de test.
End Sub
```

Notez que pour les guillemets doubles, nous utilisons `Chr(34)` car un guillemet double est le caractère ASCII 34. Ceci est nécessaire puisque les guillemets doubles sont également utilisés pour indiquer des littéraux de chaîne dans VBA.

Pour des scénarios plus nuancés où les guillemets pourraient faire partie du formatage nécessaire (par exemple, à l'intérieur d'un mot cité), une logique plus sophistiquée, impliquant peut-être Regex ou l'analyse caractère par caractère, pourrait être requise.

## Approfondissement

VBA, étant un élément essentiel dans l'automatisation des tâches au sein de la suite Microsoft Office, offre un ensemble riche de fonctions de manipulation de chaînes, `Replace` étant l'une des plus fréquemment utilisées. Cette fonction, cependant, n'effleure que la surface de ce qui peut être accompli avec VBA en termes de manipulation de chaînes.

Historiquement, VBA a hérité de ses prédécesseurs une emphase sur la simplicité pour les tâches d'automatisation de bureau, d'où l'implémentation directe de fonctions comme `Replace`. Cependant, pour les tâches de programmation modernes, surtout celles impliquant des manipulations ou des assainissements de chaînes complexes, VBA pourrait montrer ses limites.

Dans de tels cas, les programmeurs pourraient recourir à combiner VBA avec des expressions régulières (via l'objet `VBScript_RegExp_55.RegExp`) pour plus de flexibilité et de puissance dans l'analyse et la manipulation des chaînes. Cette approche, cependant, introduit une complexité supplémentaire et nécessite une solide compréhension des motifs regex, ce qui pourrait ne pas convenir à tous les utilisateurs.

Malgré ses limites, la fonction `Replace` de VBA couvre efficacement de nombreux scénarios courants impliquant la suppression de guillemets des chaînes. Elle offre une solution rapide et facile pour la plupart des besoins de manipulation de chaînes sans plonger dans le territoire regex plus complexe. Pour ceux atteignant les limites de ce que `Replace` et d'autres fonctions de chaînes basiques peuvent faire, explorer regex dans VBA ou envisager une langue plus robuste adaptée aux opérations de chaîne complexes pourrait être les prochaines meilleures étapes.
