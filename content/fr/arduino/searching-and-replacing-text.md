---
title:                "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Pourquoi

Vous êtes peut-être en train de travailler sur un projet Arduino et vous vous rendez compte que vous avez besoin de remplacer du texte dans votre code. Peut-être que vous avez accidentellement utilisé le mauvais nom pour une variable ou que vous avez besoin de modifier une constante. Quelle que soit la raison, il est important de savoir comment effectuer cette tâche facilement et efficacement.

## Comment faire

Heureusement, avec Arduino, il existe une fonction simple pour rechercher et remplacer du texte dans votre code. Il suffit d'utiliser ```replace()``` en spécifiant le texte à rechercher et le texte de remplacement.

Par exemple, si vous avez une variable nommée ```nombre``` et que vous souhaitez la remplacer par ```valeur```, vous pouvez utiliser la ligne de code suivante :

```Arduino
replace(nombre, valeur)
```

Cela remplacera toutes les occurrences de ```nombre``` par ```valeur``` dans votre code. Vous pouvez également utiliser cette fonction pour remplacer du texte dans une chaîne de caractères ou une chaîne de texte.

## Plongée en profondeur

La fonction ```replace()``` est basée sur le type de données String en Arduino. Cela signifie que vous pouvez également utiliser toutes les méthodes disponibles pour les objets String, telles que ```indexOf()``` pour trouver la position d'une chaîne de caractères.

De plus, si vous avez besoin de remplacer des caractères spéciaux dans votre code, vous pouvez utiliser des échappements de caractères tels que ```\n``` pour une nouvelle ligne ou ```\t``` pour une tabulation.

Il est également important de noter que la fonction ```replace()``` ne modifie pas la chaîne de caractères initiale, elle en crée une nouvelle. Si vous souhaitez modifier la chaîne d'origine, vous pouvez utiliser la méthode ```replace()``` en assignant le résultat à la chaîne d'origine.

# Voir aussi

Pour en savoir plus sur les fonctions et méthodes disponibles pour la manipulation de chaînes de caractères en Arduino, vous pouvez consulter les ressources suivantes :

- [Documentation officielle Arduino pour l'objet String](https://www.arduino.cc/reference/fr/language/variables/data-types/stringobject/)
- [Tutorialspoint : Les chaînes de caractères en Arduino](https://www.tutorialspoint.com/arduino/arduino_strings.htm)
- [Site officiel d'Arduino en français](https://www.arduino.cc/en/Tutorial/Strings)