---
title:    "Arduino: Suppression de caractères correspondant à un motif"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Pourquoi

Supprimer des caractères correspondants à un modèle peut être une tâche utile pour nettoyer et organiser les données dans un projet Arduino. Cela peut également être nécessaire lorsque vous travaillez avec des capteurs ou des modules qui renvoient des caractères non pertinents dans la transmission de données.

# Comment faire

Pour supprimer des caractères correspondants à un modèle en utilisant Arduino, vous pouvez suivre ces étapes simples:

1. Tout d'abord, déclarez une chaîne de caractères pour stocker la valeur complète du message reçu par le capteur ou le module.
```Arduino
String message = "ABC123DE456FGH789";
```

2. Ensuite, déclarez une nouvelle chaîne de caractères pour stocker la valeur nettoyée, en utilisant la fonction `remove()` pour supprimer les caractères correspondants à un modèle spécifié. Dans notre exemple, nous voulons supprimer tous les chiffres de la chaîne.
```Arduino
String cleaned_message = message.remove('0','9');
```

3. Enfin, imprimez la nouvelle chaîne de caractères pour voir le résultat.
```Arduino
Serial.println(cleaned_message);
```

Lors de l'exécution de ce code, la sortie affichée sur le moniteur série sera: `ABCDEFGHI`.

# Plongée profonde

La fonction `remove()` est basée sur la fonction `substring()`, qui permet de supprimer une partie spécifique d'une chaîne de caractères. Cela signifie que vous pouvez également utiliser les mêmes paramètres que pour la fonction `substring()` pour définir un modèle de caractères à supprimer. Par exemple, si vous souhaitez supprimer tous les caractères après le quatrième caractère, vous pouvez utiliser la fonction `remove()` comme ceci:
```Arduino
String message = "This is a long message";
String cleaned_message = message.remove(4);

Serial.println(cleaned_message);
```

La sortie sera: `This`.

# Voir aussi

- [Documentation officielle de la fonction `remove()` en anglais](https://www.arduino.cc/en/Tutorial/StringRemove)
- [Exemples de la fonction `substring()` en anglais](https://www.arduino.cc/en/Reference/StringSubstring)