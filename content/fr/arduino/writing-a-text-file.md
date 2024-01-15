---
title:                "Ecrire un fichier texte"
html_title:           "Arduino: Ecrire un fichier texte"
simple_title:         "Ecrire un fichier texte"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous utilisez un Arduino, il y a de fortes chances que vous vouliez enregistrer de l'information pour une utilisation future. L'un des moyens les plus simples de le faire est d'écrire un fichier texte. Mais comment y arriver?

## Comment faire

```Arduino
File myFile;
myFile = SD.open("fichier.txt", FILE_WRITE);
if(myFile){
  myFile.println("Ceci est un exemple de texte.");
  myFile.close();
}
```

Ce code ouvre un fichier texte appelé "fichier.txt" et écrit une ligne de texte à l'intérieur. Assurez-vous d'avoir une carte SD insérée dans votre Arduino pour que cela fonctionne. Vous pouvez ensuite utiliser un lecteur de carte SD pour accéder au fichier et voir le texte enregistré.

## Plongée en profondeur

Il est important de noter que la méthode "FILE_WRITE" utilisée dans l'exemple précédent écrira par-dessus un fichier existant s'il en trouve un avec le même nom. Pour éviter cela, vous pouvez utiliser la méthode "FILE_WRITE" avec un nom de fichier différent à chaque fois.

Il est également possible d'écrire plusieurs lignes de texte dans un fichier en utilisant une boucle for, par exemple:

```Arduino
File myFile;
myFile = SD.open("fichier.txt", FILE_WRITE);
if(myFile){
  for(int i = 0; i < 5; i++){
    myFile.println("Ceci est une ligne de texte.");
  }
  myFile.close();
}
```

Cela écrira cinq lignes de texte identiques dans le fichier "fichier.txt". Vous pouvez également utiliser des variables pour rendre le texte plus dynamique, par exemple:

```Arduino
File myFile;
myFile = SD.open("fichier.txt", FILE_WRITE);
if(myFile){
  int temperature = 25;
  myFile.println("La température actuelle est de " + String(temperature) + " degrés.");
  myFile.close();
}
```

Cela écrira une ligne de texte avec la température actuelle, telle que "La température actuelle est de 25 degrés."

## Voir aussi

Pour en savoir plus sur la manipulation de fichiers texte avec un Arduino, vous pouvez consulter les liens suivants:

- [Utilisation de la carte SD avec Arduino](https://www.arduino.cc/en/Reference/SD)
- [Guide de référence pour les méthodes d'écriture de fichiers SD](https://www.arduino.cc/en/Reference/FileWrite)
- [Tutoriel de Sparkfun sur l'enregistrement de données avec un Arduino et une carte SD](https://learn.sparkfun.com/tutorials/logging-data-with-arduino)