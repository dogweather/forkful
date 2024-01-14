---
title:    "Arduino: Impression de la sortie de débogage"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation Arduino, vous pourriez vous demander pourquoi il est important d'afficher les résultats de débogage lors de la programmation. La réponse est simple - le débogage vous aide à comprendre le processus de votre programme et à identifier les erreurs potentielles. Cela peut vous faire gagner beaucoup de temps et vous éviter des frustrations plus tard.

## Comment faire

La première étape pour afficher les résultats de débogage consiste à utiliser la fonction `Serial.begin()` dans votre code Arduino. Cela vous permet de communiquer avec l'IDE Arduino et d'afficher des informations sur le port série. Vous devez également définir la vitesse de communication, généralement en bauds, en utilisant `Serial.begin(baudrate)`.

Voici un exemple de code avec une sortie de débogage :

```Arduino
void setup() {
  Serial.begin(9600); //définit la vitesse de communication à 9600 bauds
}

void loop() {
  int valeur = 5;
  Serial.println("La valeur est : " + String(valeur)); //affiche la valeur sur le port série
}
```

La ligne `Serial.println()` est utilisée pour afficher la valeur sur le port série. Vous pouvez également utiliser `Serial.print()` pour afficher la valeur sans ajouter de nouvelle ligne à la fin.

Une fois que vous avez téléchargé ce code sur votre Arduino et ouvert le moniteur série dans l'IDE Arduino, vous verrez que la valeur est affichée chaque fois que la boucle `loop()` est exécutée.

```
La valeur est : 5
La valeur est : 5
La valeur est : 5
...
```

Cela peut sembler simple, mais cela peut être très utile pour déboguer votre code et vérifier si les valeurs sont correctement attribuées.

## Plongée en profondeur

En plus de simplement afficher des valeurs, vous pouvez également utiliser le débogage pour identifier les erreurs dans votre code. Par exemple, vous pouvez utiliser `Serial.println()` pour afficher des messages à des moments clés de votre code afin de suivre son exécution et de comprendre où les erreurs se produisent. Vous pouvez également utiliser `Serial.print()` pour afficher des valeurs de variables à des moments spécifiques pour vérifier si elles sont correctement attribuées.

Il est également possible de formater la sortie de débogage en utilisant des balises de formatage pour afficher des valeurs avec une précision spécifique. Par exemple, `Serial.println("La température est : " + String(temperature, DEC))` affichera la valeur de la variable `temperature` avec une précision de nombre décimal.

## Voir aussi

- [Documentation officielle de la fonction Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Vidéo tutoriel sur l'utilisation du débogage avec Arduino](https://www.youtube.com/watch?v=QEzmz21PVd0)
- [Tutoriel sur le débogage avec Visual Studio Code et Arduino](https://github.com/arduino/arduino-ide/blob/master/doc/debugger.md)