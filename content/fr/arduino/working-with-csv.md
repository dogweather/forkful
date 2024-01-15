---
title:                "Travailler avec des fichiers csv"
html_title:           "Arduino: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Arduino, vous avez probablement déjà entendu parler du format de données CSV. Mais pourquoi travailler avec des fichiers CSV en premier lieu ? Eh bien, CSV est un format populaire pour stocker des données tabulaires simples telles que des tableaux ou des feuilles de calcul. Cela peut être très utile si vous souhaitez utiliser des données externes dans vos projets Arduino.

## Comment faire

Pour commencer à travailler avec des fichiers CSV dans votre code Arduino, vous aurez besoin d'une librairie externe appelée "ArduinoCSV". Vous pouvez l'installer en ouvrant l'IDE Arduino, en allant dans "Outils > Gérer les bibliothèques" et en recherchant "ArduinoCSV".

Une fois la librairie installée, vous pouvez l'utiliser dans votre code en l'important avec la ligne suivante :

```Arduino
#include <ArduinoCSV.h>
```

Ensuite, vous pouvez créer un objet CSV en spécifiant le numéro de la broche sur lequel se trouve votre fichier CSV :

```Arduino
CSV csv;
csv = CSV(10); // Utiliser la broche numéro 10
```

Vous pouvez ensuite charger votre fichier CSV à partir d'un serveur en utilisant la méthode "readFromURL" :

```Arduino
csv.readFromURL("https://exemple.com/monfichier.csv");
```

Enfin, vous pouvez parcourir et accéder aux données de votre fichier CSV en utilisant des boucles et des indices :

```Arduino
for (int i = 0; i < csv.rows(); i++) {
  // Accéder à la donnée de la ligne i et de la colonne 2
  String donnée = csv.getField(i, 2);
}
```

Le tableau suivant résume les méthodes les plus utiles de la librairie ArduinoCSV :

Méthode | Description
--------|------------
readFromURL(String url) | Charge un fichier CSV à partir d'un serveur.
rows() | Renvoie le nombre de lignes dans le fichier CSV.
fields() | Renvoie le nombre de colonnes dans le fichier CSV.
getField(int row, int col) | Renvoie la donnée d'une cellule spécifique dans le fichier CSV.

## Plongée en profondeur

Maintenant que vous avez une idée générale de comment travailler avec des fichiers CSV dans Arduino, voici quelques informations supplémentaires pour vous aider à mieux comprendre le format de données.

CSV signifie Comma-Separated Values, ce qui signifie que les données sont séparées par des virgules (ou par d'autres délimiteurs tels que des points-virgules). Les cellules sont représentées comme des chaînes de caractères, et les lignes sont séparées par des retours à la ligne.

Il est important de noter que les numéros de ligne et de colonne dans la librairie ArduinoCSV commencent à 0, alors que dans la plupart des feuilles de calcul, ils commencent à 1.

Enfin, vous pouvez également écrire des fichiers CSV à partir de Arduino en utilisant la méthode "writeToFile" :

```Arduino
csv.writeToFile("monfichier.csv");
```

## Voir aussi

- [Documentation de la librairie ArduinoCSV](https://github.com/arduino-libraries/ArduinoCSV)
- [Tutoriel vidéo sur l'utilisation de la librairie ArduinoCSV](https://www.youtube.com/watch?v=sk1eTmQbJyc)
- [Exemple de projet Arduino utilisant des données CSV](https://www.hackster.io/arduino/libraries-arduino-csv-f655e5)