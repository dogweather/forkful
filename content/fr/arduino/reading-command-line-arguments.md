---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Arduino: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un passionné de programmation et que vous souhaitez utiliser votre Arduino pour de nouveaux projets, savoir comment lire les arguments de la ligne de commande peut être très utile. Cela vous permettra de créer des programmes plus flexibles et dynamiques en utilisant des valeurs que vous pouvez passer en tant qu'arguments.

## Comment faire

Pour lire les arguments de la ligne de commande sur votre Arduino, vous devrez utiliser la fonction `Serial.readString()` pour capturer les entrées provenant du moniteur série. Ensuite, vous pourrez manipuler ces entrées en utilisant des instructions conditionnelles et d'autres fonctions, selon ce que vous souhaitez en faire. Voici un exemple de code pour lire deux arguments (nom et âge) et les afficher sur le moniteur série :

```
Arduino String nom;
int age;

void setup()
{
  Serial.begin(9600);
  while (!Serial) {
    ; // Attendre la connexion avec le moniteur série.
  }
}
 
void loop() 
{
  if (Serial.available()) {
    nom = Serial.readString(); // Lire le premier argument (nom).
    age = Serial.parseInt(); // Lire le deuxième argument (âge) et le convertir en entier.
    Serial.print("Bonjour "); // Afficher le nom.
    Serial.print(nom);
    Serial.print(", tu as ");
    Serial.print(age);
    Serial.print(" ans.");
  }
}
```

Si vous entrez "Jean 25" dans le moniteur série, le résultat affichera "Bonjour Jean, tu as 25 ans."

## Plongée plus profonde

La fonction `Serial.readString()` lit toute la chaîne jusqu'au premier caractère de fin de ligne (\n). Si vous souhaitez lire seulement une partie de la chaîne ou utiliser un caractère de fin de ligne différent, vous pouvez utiliser la fonction `Serial.readStringUntil()` en spécifiant le caractère souhaité comme paramètre. De plus, si vous souhaitez récupérer des valeurs numériques précises en utilisant `Serial.parseInt()`, vous devez vous assurer que votre chaîne ne contient que des chiffres. Sinon, vous obtiendrez une valeur de 0.

## Voir aussi

- [Documentation officielle d'Arduino sur Serial.readString()](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstring/) 
- [Documentation officielle d'Arduino sur Serial.parseInt()](https://www.arduino.cc/reference/en/language/functions/conversion/parseint/)
- [Tutoriel vidéo sur la lecture des arguments de la ligne de commande avec Arduino](https://www.youtube.com/watch?v=JAt43TSgzNM)