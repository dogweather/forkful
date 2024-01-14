---
title:                "Arduino: Passage d'une chaîne de caractères en minuscules"
simple_title:         "Passage d'une chaîne de caractères en minuscules"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles on peut avoir besoin de convertir une chaîne de caractères en minuscules dans un programme Arduino. Cela peut être utile pour la comparaison de chaînes de caractères ou pour faciliter l'affichage de texte sur un écran. Dans cet article, nous allons explorer comment convertir une chaîne de caractères en minuscules dans un programme Arduino.

## Comment faire

Pour convertir une chaîne de caractères en minuscules, nous allons utiliser la méthode `toLowerCase()` de l'objet `String` dans Arduino. Voici un exemple de code qui prend une chaîne de caractères en entrée, la convertit en minuscules et l'affiche sur le moniteur série :

```Arduino
void setup() {
	Serial.begin(9600);  // Initialise le moniteur série
}

void loop() {
	String texte = "Bonjour Arduino"; // Chaîne de caractères à convertir

	String texteEnMinuscules = texte.toLowerCase(); // Utilise toLowerCase() pour convertir en minuscules

	Serial.println(texteEnMinuscules); // Affiche le texte converti sur le moniteur série

	delay(1000); // Attends une seconde avant de recommencer
}
```

Lorsque nous exécutons ce programme, nous obtenons l'output suivant sur le moniteur série :

```
bonjour arduino
```

Vous pouvez également utiliser cette méthode pour convertir directement une chaîne de caractères stockée dans une variable :

```Arduino
void setup() {
	Serial.begin(9600);  // Initialise le moniteur série
}

void loop() {
	String texte = "Bonjour Arduino"; // Chaîne de caractères à convertir
	Serial.println(texte); // Affiche la chaîne de caractères originale
	texte.toLowerCase(); // Convertit en minuscules et remplace la valeur de la variable texte
	Serial.println(texte); // Affiche la nouvelle version en minuscules
	delay(1000); // Attends une seconde avant de recommencer
}
```

L'output sera le même que dans le premier exemple.

## Plongée en profondeur

La méthode `toLowerCase()` de l'objet `String` utilise en réalité la fonction `tolower()` de la bibliothèque standard de C++. Cette fonction prend en entrée un caractère et le convertit en minuscule si c'est un caractère alphabétique. Dans le cas contraire, elle renvoie simplement le caractère d'entrée sans le modifier.

Il est important de noter que cette méthode ne modifie pas la chaîne de caractères originale, mais renvoie une nouvelle chaîne de caractères avec la conversion en minuscules. Il est donc nécessaire d'utiliser une variable pour stocker cette nouvelle chaîne de caractères si vous souhaitez la réutiliser.

## Voir aussi

- [La méthode `toLowerCase()` de l'objet `String`](https://www.arduino.cc/en/Reference/StringToLowercase)
- [La fonction `tolower()` de la bibliothèque standard de C++](http://www.cplusplus.com/reference/cctype/tolower/)