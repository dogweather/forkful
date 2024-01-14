---
title:                "Arduino: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières ou "regular expressions" en anglais, sont un outil puissant pour rechercher et manipuler des motifs dans du texte. Ils sont largement utilisés dans des domaines tels que la programmation, la validation de données, et même l'analyse de données. Apprendre à utiliser les expressions régulières peut vous permettre d'économiser du temps et des efforts dans votre code Arduino.

## Comment faire

Pour utiliser des expressions régulières dans votre code Arduino, vous devez d'abord inclure la bibliothèque `Regex` en ajoutant la ligne suivante au début de votre code :

```Arduino
#include <Regex.h>
```

Ensuite, vous pouvez déclarer une variable de type `Regex` en utilisant la syntaxe suivante :

```Arduino
Regex nom_variable(expression_régulière);
``` 

Par exemple, si vous voulez vérifier si une chaîne de caractères correspond à un motif particulier, vous pouvez utiliser la fonction `match()` de la bibliothèque `Regex` comme ceci :

```Arduino
Regex mon_regexp("[a-z]+");
if(mon_regexp.match("test")) {
  Serial.println("Correspond !");
}
```

Dans cet exemple, la chaîne "test" correspond au motif `[a-z]+` qui signifie "une ou plusieurs lettres minuscules". Vous pouvez également utiliser des expressions régulières pour extraire des informations spécifiques d'une chaîne de caractères, par exemple pour récupérer un nombre ou un mot précis.

## Plongée en profondeur

Les expressions régulières peuvent sembler compliquées à première vue, mais une fois que vous avez compris leur syntaxe et leurs règles, vous pouvez les utiliser de manière très efficace. Voici quelques éléments de syntaxe à retenir :

- Les crochets `[ ]` délimitent un ensemble de caractères qui peuvent correspondre à un seul caractère dans la chaîne de recherche.
- L'astérisque `*` signifie "zéro ou plusieurs fois".
- Le signe plus `+` signifie "une ou plusieurs fois".
- Le point `.` correspond à n'importe quel caractère.
- L'antislash `\` est utilisé pour échapper certains caractères spéciaux tels que les crochets ou les astérisques.

Pour en savoir plus sur les expressions régulières et leur utilisation en détail, vous pouvez consulter les liens recommandés ci-dessous.

## Voir aussi

- [Documentation officielle de la bibliothèque Regex](https://www.arduino.cc/reference/en/libraries/regex/)
- [Tutorial sur les expressions régulières en C++ (en anglais)](https://www.regextutorial.org/)
- [Utilisation des expressions régulières dans l'analyse de données (en français)](https://openclassrooms.com/fr/courses/257191-apprenez-a-programmer-en-python/2236844-analysez-des-donnees-textuelles-avec-les-expressions-regulieres)