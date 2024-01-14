---
title:    "Arduino: Vérifier l'existence d'un répertoire"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi 

Vérifier l'existence d'un répertoire peut être utile pour s'assurer que le chemin d'accès est correct avant d'y accéder ou pour vérifier s'il y a suffisamment d'espace disponible avant de créer de nouveaux fichiers.

## Comment Faire 

```Arduino
#include <SD.h> 

if(SD.exists("/monRepertoire")) { 
    Serial.println("Le répertoire existe."); 
} else { 
    Serial.println("Le répertoire n'existe pas."); 
}
```

Dans cet exemple, nous utilisons la bibliothèque SD pour vérifier si le répertoire "monRepertoire" existe sur la carte SD. Si c'est le cas, nous affichons un message indiquant son existence, sinon nous affichons un message indiquant qu'il n'existe pas.

## Plongée Profonde 

Pour vérifier l'existence d'un répertoire, nous utilisons la fonction "exists" de la bibliothèque SD. Cette fonction prend en paramètre le chemin d'accès du répertoire à vérifier et renvoie une valeur booléenne (true ou false) en fonction de son existence.

Il est également possible de vérifier l'existence d'un fichier en utilisant la fonction "exists" avec le chemin d'accès du fichier.

Il est important de noter que cette fonction ne vérifie pas si le répertoire ou le fichier est vide, elle vérifie simplement son existence.

## Voir Aussi 

- Tutoriel sur l'utilisation de la bibliothèque SD sur Arduino : https://www.arduino.cc/en/Reference/SD
- Documentation complète de la fonction "exists" : https://www.arduino.cc/en/Reference/SDexists