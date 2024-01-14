---
title:    "Arduino: Lecture d'un fichier texte"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire des fichiers texte est une compétence importante à maîtriser lorsqu'on programme avec Arduino. Cela permet de stocker de grandes quantités de données qui peuvent être utilisées dans nos projets. Dans cet article, je vais vous montrer comment lire un fichier texte avec Arduino et vous fournir des exemples de code pour vous aider à démarrer.

## Comment faire

Tout d'abord, nous devons définir un objet de type fichier pour accéder au fichier texte. Ensuite, nous pouvons utiliser la fonction `open()` pour ouvrir le fichier et la fonction `read()` pour lire son contenu. Voici un exemple de code pour lire un fichier texte nommé "data.txt" et afficher son contenu:

```Arduino
File file = SD.open("data.txt"); //définir l'objet fichier et ouvrir le fichier
while (file.available()) { //tant qu'il y a du contenu à lire
    Serial.println(file.read()); //afficher le contenu
}
file.close(); //fermer le fichier
```

Lors de l'exécution de ce code, l'output sur le moniteur série sera le contenu du fichier texte, ligne par ligne.

## Plongée en profondeur

Sans entrer dans les détails techniques complexes, il est important de noter que le fichier texte doit être stocké sur une carte SD pour être lu par Arduino. De plus, il est possible de spécifier un chemin de fichier spécifique en utilisant la fonction `open()` si le fichier n'est pas situé dans le répertoire racine de la carte SD.

Il est également possible de lire des fichiers texte plus longs en utilisant la fonction `readBytes()` qui permet de spécifier le nombre maximum de caractères à lire à la fois. De plus, la fonction `seek()` peut être utilisée pour déplacer le curseur de lecture dans le fichier, ce qui peut être utile pour lire des données spécifiques.

Le code présenté dans la section précédente peut également être adapté pour lire des données provenant de capteurs ou d'autres sources et les enregistrer dans un fichier texte pour une utilisation ultérieure.

## Voir aussi

Pour plus d'informations sur la lecture de fichiers texte avec Arduino, voici quelques liens utiles:

- https://www.arduino.cc/en/Reference/SDOpen
- https://www.arduino.cc/en/Reference/SDRead
- https://www.arduino.cc/en/Reference/SDReadBytes
- https://www.arduino.cc/en/Reference/SDSeek