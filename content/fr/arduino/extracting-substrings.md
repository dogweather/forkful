---
title:                "Arduino: Extraction de sous-chaînes"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous travaillez sur un projet Arduino qui implique des chaînes de caractères, vous devrez peut-être extraire une partie spécifique de cette chaîne. Pour ce faire, vous devrez utiliser la fonction d'extraction de sous-chaîne dans votre code. Dans cet article, nous allons vous expliquer comment le faire en utilisant l'IDE Arduino et vous donner une vue d'ensemble de son fonctionnement.

# Comment faire

Pour extraire une sous-chaîne dans Arduino, vous pouvez utiliser la fonction `substring()`. Cette fonction prend deux paramètres: l'index de départ et la longueur de la sous-chaîne à extraire. Par exemple, si nous avons la chaîne "Bonjour tout le monde" et que nous voulons extraire la sous-chaîne "tout le monde", nous utiliserions la fonction de cette manière:

```arduino
String str = "Bonjour tout le monde";
String sub = str.substring(8, 12);
```

La variable `str` contient la chaîne complète, tandis que la variable `sub` contient la sous-chaîne extraite qui commence à l'index 8 et a une longueur de 12 caractères.

Dans l'exemple ci-dessus, nous avons utilisé des valeurs statiques pour l'index et la longueur, mais vous pouvez également les remplacer par des variables. Cela vous permet de changer facilement la sous-chaîne extraite en fonction des besoins de votre projet.

# Plongée en profondeur

La fonction `substring()` peut également être utilisée pour extraire des sous-chaînes à partir d'autres types de données tels que des tableaux de caractères ou des tableaux de chaînes. Vous pouvez également utiliser des expressions régulières pour spécifier la sous-chaîne à extraire.

Il est important de noter que la fonction `substring()` ne modifie pas la chaîne d'origine et ne renvoie qu'une copie de la sous-chaîne extraite. Si vous souhaitez modifier la chaîne d'origine, vous devrez utiliser la fonction `replace()`.

# Voir aussi

- [Documentation officielle sur la fonction substring() dans l'IDE Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Tutoriel vidéo sur l'extraction de sous-chaînes dans Arduino](https://www.youtube.com/watch?v=C1p82_8-GqE)
- [Exemples de projets Arduino utilisant la fonction substring()](https://create.arduino.cc/projecthub/projects/tags/substring)

Merci d'avoir lu cet article sur l'extraction de sous-chaînes dans Arduino. Nous espérons que cela vous a été utile dans vos projets futurs ! Bon codage !