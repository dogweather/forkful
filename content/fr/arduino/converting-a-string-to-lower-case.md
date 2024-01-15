---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il serait utile de convertir une chaîne de caractères en minuscules dans votre code Arduino. La réponse est simple : cela peut rendre votre code plus facile à lire et à comprendre. En utilisant une seule casse pour vos variables et vos chaînes de caractères, vous évitez les confusions et les erreurs de saisie.

## Comment faire

Pour convertir une chaîne de caractères en minuscules dans votre code Arduino, vous pouvez utiliser la fonction "toLowerCase()". Cette fonction prend en paramètre une chaîne de caractères et renvoie une nouvelle chaîne avec toutes les lettres en minuscules. Regardons un exemple :

```Arduino
String str = "Bonjour";
Serial.println(str.toLowerCase());
```

Résultat : "bonjour"

Vous pouvez également l'utiliser avec des données provenant de capteurs ou de modules, par exemple :

```Arduino
String input = Serial.readString();
Serial.println(input.toLowerCase());
```

Résultat : si vous envoyez "HELLO" via la communication série, le résultat sera "hello".

## Plongée plus profonde

Il peut également être utile de comprendre comment fonctionne la fonction "toLowerCase()" sous le capot. Dans Arduino, les chaînes de caractères sont en fait des objets de la classe "String", qui possède plusieurs méthodes, dont "toLowerCase()". Cette méthode utilise une boucle pour parcourir chaque caractère de la chaîne et le convertir en minuscule en utilisant la fonction "tolower()". La nouvelle chaîne est ensuite renvoyée.

Il est important de noter que la fonction "toLowerCase()" prend en compte uniquement les lettres de l'alphabet. Les caractères spéciaux, les nombres et les symboles resteront inchangés.

## Voir aussi

Vous pouvez consulter notre article sur les classes et les objets en Arduino pour en savoir plus sur la classe "String" et ses méthodes.

Voir aussi :

- Article sur les classes et les objets en Arduino : lien vers l'article en anglais
- Référence complète de la classe String : lien vers la documentation officielle en anglais