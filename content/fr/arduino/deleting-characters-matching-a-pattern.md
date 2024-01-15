---
title:                "Suppression des caractères correspondant à un motif"
html_title:           "Arduino: Suppression des caractères correspondant à un motif"
simple_title:         "Suppression des caractères correspondant à un motif"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi?

Supprimer des caractères correspondant à un motif peut être utile lors de la manipulation de données ou de chaînes de caractères, en permettant au code de fonctionner sur une entrée plus propre ou en supprimant des informations inutiles.

## Comment faire?

Pour supprimer des caractères correspondant à un motif dans un code Arduino, vous pouvez utiliser la fonction `remove_if()` de la bibliothèque `String`. Cette fonction prend en paramètres une chaîne de caractères, une fonction de prédicat et un caractère de substitution. Elle parcourt la chaîne de caractères et supprime les caractères qui satisfont le prédicat, en les remplaçant par le caractère de substitution.

```
Arduino

#include <String.h>
String data = "Bonjour le monde !";
data.remove_if(isDigit, '.');
Serial.println(data); // Output: Bonjour le monde !
```

Dans cet exemple, nous utilisons la fonction `isDigit()` qui renvoie `true` si le caractère est un chiffre. Cette fonction est prédéfinie dans la bibliothèque `String`. Nous passons également le caractère `'.'` comme caractère de substitution afin de remplacer les chiffres par des points.

Vous pouvez également créer votre propre fonction de prédicat personnalisée en utilisant la syntaxe suivante :

```
bool isVowel(char c) {
  return (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u');
}

data.remove_if(isVowel, '*');
Serial.println(data); // Output: B*nj**r l* m*nd* !
```

Dans cet exemple, nous utilisons notre propre fonction `isVowel()` pour supprimer toutes les voyelles dans la chaîne de caractères et les remplacer par des astérisques.

## Plongée profonde

La fonction `remove_if()` parcourt la chaîne de caractères de gauche à droite et supprime les caractères qui satisfont le prédicat. Il est important de noter que la chaîne de caractères est modifiée en place, donc si vous avez besoin de conserver l'original, vous devrez créer une copie avant d'appeler la fonction.

De plus, cette fonction ne supprime pas les caractères blancs. Si vous souhaitez également supprimer les espaces, vous devrez les ajouter au prédicat ou utiliser une autre fonction comme `replace()`.

```
String data = "   test    ";
data.remove_if(isSpace);
Serial.println(data); // Output:   test
data.replace(" ", "");
Serial.println(data); // Output: test
```

## Voir aussi

- [Documentation Arduino pour la fonction `remove_if()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/remove_if/)