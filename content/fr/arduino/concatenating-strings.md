---
title:                "Arduino: La concaténation de chaînes"
simple_title:         "La concaténation de chaînes"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# Pourquoi

Dans la programmation Arduino, il est souvent nécessaire de combiner plusieurs chaînes de caractères pour créer une phrase ou un message complet. La concaténation de chaînes est une technique fréquemment utilisée pour cela. Dans cet article, nous allons découvrir comment concaténer des chaînes dans nos projets Arduino.

# Comment faire

La concaténation de chaînes dans Arduino peut être réalisée en utilisant l'opérateur "+" ou en utilisant la fonction "concat". Voici un exemple de code qui utilise l'opérateur "+":

```Arduino
String name = "Jean";
String age = "30";
String message = "Bonjour, je m'appelle " + name + " et j'ai " + age + " ans.";
Serial.println(message);
```

Cela produira la sortie suivante:

```
Bonjour, je m'appelle Jean et j'ai 30 ans.
```

On peut également utiliser la fonction "concat" pour concaténer des chaînes:

```Arduino
String name = "Marie";
String age = "25";
String message = "Bonjour, je m'appelle ";
message.concat(name);
message.concat(" et j'ai ");
message.concat(age);
message.concat(" ans.");
Serial.println(message);
```

Cette fois-ci, la sortie sera toujours la même:

```
Bonjour, je m'appelle Marie et j'ai 25 ans.
```

# Plongée en profondeur

Lorsque vous utilisez l'opérateur "+" pour concaténer des chaînes, il est important de noter que les deux opérandes doivent être des objets de type String. Si vous utilisez des variables de type int ou char, vous devrez les convertir en chaînes de caractères avant de les concaténer.

De plus, si vous concaténez un grand nombre de chaînes, il peut être plus efficace d'utiliser la fonction "concat" plutôt que l'opérateur "+". En effet, la fonction "concat" a été spécialement conçue pour gérer efficacement la concaténation de chaînes dans Arduino.

Enfin, il est important de noter que la concaténation de chaînes peut être consommatrice de mémoire et peut ralentir votre code. Il est donc recommandé de minimiser son utilisation et de l'utiliser uniquement lorsque c'est vraiment nécessaire.

# Voir aussi

- [Documentation officielle Arduino sur la concaténation de chaînes](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Tutoriel vidéo sur la concaténation de chaînes dans Arduino](https://www.youtube.com/watch?v=K1oO4g47YVg)
- [Guide de référence complet pour la programmation Arduino](https://www.arduino.cc/reference/en/)

Merci d'avoir lu cet article sur la concaténation de chaînes dans Arduino. Nous espérons que cela vous sera utile dans vos futurs projets. À bientôt pour d'autres astuces et techniques de programmation Arduino !