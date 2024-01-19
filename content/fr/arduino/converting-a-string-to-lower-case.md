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

## Qu'est-ce que c'est & Pourquoi ?
Convertir une chaîne de caractères en minuscules est le processus de transformation de tous les caractères d'une chaîne de texte en lettres minuscules. Les programmeurs font cela pour notamment uniformiser les données textuelles afin de favoriser l'égalité de comparaison.

## Comment faire :
Voici un exemple de code pour convertir une string en minuscule sur Arduino :

```Arduino
char monMessage[] = "Bonjour, ARDUINO!";

void setup() {
  Serial.begin(9600);
  for (int i = 0; monMessage[i]; i++) {
    monMessage[i] = toLowerCase(monMessage[i]);
    Serial.print(monMessage[i]);
  }
}

void loop() {
  // rien pour le moment!
}
```

Une fois ce code exécuté, votre sortie de série affichera :

```Arduino
"bonjour, arduino!"
```

## Plongée en Profondeur
Historiquement, la conversion de chaînes en minuscules a été utilisée pour normaliser les données et améliorer la précision des comparaisons. Dans le monde de l'Arduino, cela peut être particulièrement utile pour comparer des entrées utilisateur, où les lettres majuscules ou minuscules peuvent être entrées de manière aléatoire.

En termes d'alternatives, on peut aussi utiliser une fonction personnalisée pour effectuer la conversion. Cependant, utiliser `toLowerCase()` est généralement plus simple et plus rapide.

En ce qui concerne les détails de mise en œuvre, la fonction `toLowerCase()` examine chaque caractère de la chaîne un par un, vérifie s'il s'agit d'une lettre majuscule, et si c'est le cas, la convertit en minuscule.

## Voir Aussi
Pour plus d'informations sur la programmation Arduino, les chaînes de caractères et la conversion de types, consultez ces ressources :

- Documentation officielle d'Arduino sur les chaînes de caractères : https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Guide pratique pour travailler avec des chaînes de caractères en Arduino : https://create.arduino.cc/projecthub/project14/strings-a-primer-1f1990
- Plus d'informations sur `toLowerCase()` : https://www.arduino.cc/reference/tr/language/variables/data-types/string/functions/tolowercase/