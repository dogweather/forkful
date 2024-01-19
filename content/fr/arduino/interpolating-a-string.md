---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ? 

L'interpolation de chaîne est une manière d'insérer des valeurs de variables directement dans une chaîne de caractères. Les programmeurs l’utilisent pour créer des chaînes de caractères plus lisibles et ordonnées.

## Comment faire : 

Dans Arduino, on utilise souvent la fonction `sprintf()` pour interpoler des chaînes. Admettons que vous ayez une variable entière `age` et une chaîne `name`, voici comment ça se fait :

```Arduino
char name[] = "Pierre";
int age = 23;

char buffer[50];
sprintf(buffer, "Bonjour, Je suis %s et j’ai %d ans.", name, age);

Serial.begin(9600);
Serial.println(buffer);
```

Cela affiche : "Bonjour, Je suis Pierre et j’ai 23 ans."

## Plongée en profondeur : 

Historiquement, l'interpolation de chaîne était une caractéristique majeure des langages de programmation de scripts tels que Perl et Ruby. En Arduino, nous utilisons la fonction C `sprintf()`. 

Il y a des alternatives à `sprintf()`, comme `snprintf()`, qui vérifie que vous n'écrivez pas plus de caractères que le tampon peut en contenir.

En ce qui concerne les détails, `sprintf()` fonctionne en analysant la chaîne pour y trouver des caractères de format, comme `%s` pour les chaînes, et `%d` pour les entiers. Une fois trouvés, `sprintf()` les remplace par les valeurs des variables que vous avez spécifiées.

## Voir aussi :

Pour plus d'informations sur `sprintf()` et `snprintf()`, consultez les liens suivants :
- `sprintf()`: http://www.cplusplus.com/reference/cstdio/sprintf/
- `snprintf()`: http://www.cplusplus.com/reference/cstdio/snprintf/ 

Et bien sûr, la référence Arduino : http://arduino.cc/en/Reference/HomePage