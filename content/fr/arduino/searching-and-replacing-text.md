---
title:                "Arduino: Recherche et remplacement de texte"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Quand vous écrivez du code pour votre projet Arduino, il peut arriver que vous ayez besoin de modifier ou remplacer une partie du texte pour le rendre plus précis ou fonctionnel. Dans cet article, nous allons vous montrer comment utiliser la fonction de recherche et de remplacement dans votre code Arduino.

## Comment faire

Pour commencer, nous devons d'abord ajouter la bibliothèque "String" à notre code en utilisant la ligne suivante :

```Arduino
#include <String.h>
```

Ensuite, nous déclarons une variable de type "String" qui contiendra notre texte d'origine :

```Arduino
String texte = "Bonjour tout le monde.";
```

Maintenant, nous pouvons utiliser la fonction "replace()" pour remplacer une partie du texte. Par exemple, si nous voulons remplacer "tout le" par "les", nous pouvons utiliser la ligne suivante :

```Arduino
texte.replace("tout le", "les");
```

Lorsque nous imprimons la variable "texte", nous obtiendrons le résultat suivant :

```
Bonjour les monde.
```

Nous pouvons également utiliser la fonction "indexOf()" pour rechercher la position d'un mot ou d'une phrase spécifique dans notre texte. Par exemple :

```Arduino
int position = texte.indexOf("monde");
```

Cela nous donnera la position du mot "monde" dans notre texte, dans ce cas, la valeur de "position" sera de 11.

## Plongée en profondeur

Il est important de noter que la fonction "indexOf()" recherche le texte dans notre variable en commençant par l'indice 0. Cela signifie que la première lettre de notre texte aura une position de 0, et ainsi de suite. De plus, la fonction "replace()" ne changera que la première occurrence du texte spécifié.

Il est également possible d'utiliser des boucles pour rechercher et remplacer du texte de manière itérative dans une chaîne de caractères plus grande.

## Voir aussi

- Référence officielle d'Arduino : https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Tutoriel sur les chaînes de caractères dans Arduino : https://www.technipages.com/arduino-string-how-to
- Exemples de codes pour la fonction "replace()" : https://www.techonthenet.com/arduino/functions/string/replace.php