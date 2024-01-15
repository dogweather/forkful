---
title:                "Travailler avec yaml"
html_title:           "Arduino: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous voulez stocker et organiser des données en toute simplicité, YAML est le bon choix. C'est un format de données lisible par les humains et facilement manipulable par les machines, ce qui le rend très pratique pour l'utilisation avec Arduino.

## Comment faire

Pour utiliser YAML avec Arduino, vous devez d'abord inclure la bibliothèque adaptée dans votre code. Voici un exemple de code pour vous montrer comment utiliser YAML pour stocker une liste de noms :

```Arduino
#include <YAML.h>

// Créez une variable qui contiendra vos données
YAML::Node noms;

// Ajoutez des valeurs à votre variable
noms["nom1"] = "Jean";
noms["nom2"] = "Marie";
noms["nom3"] = "Paul";

// Écrivez vos données dans un fichier YAML nommé "noms.yml"
File f = SD.open("noms.yml", FILE_WRITE);
YAML::Printer printer(f);
printer.print(noms);
f.close();

// Lisez vos données depuis le fichier et affichez-les dans la console série
File f = SD.open("noms.yml", FILE_READ);
YAML::Parser parser(f);
while(parser.next()) {
  parser.print();
}
f.close();
```

Résultat de la console série :

```
nom1: Jean
nom2: Marie
nom3: Paul
```

## Plongez plus profondément

YAML utilise une syntaxe simple avec des indentations pour représenter la structure des données. Par exemple, si vous voulez stocker une liste de noms avec leurs âges, vous pouvez utiliser ce format :

```YAML
noms:
  - nom: Jean
    age: 25
  - nom: Marie
    age: 32
  - nom: Paul
    age: 28
```

Vous pouvez également utiliser YAML pour stocker des objets avec des propriétés en utilisant des tirets au lieu de points :

```YAML
personne1:
  nom: Jean
  age: 25
personne2:
  nom: Marie
  age: 32
personne3:
  nom: Paul
  age: 28
```

Pour en savoir plus sur la syntaxe YAML, vous pouvez consulter la documentation officielle [ici] (https://yaml.org/spec/1.2/spec.html) ou faire des recherches en ligne.

## Voir aussi

- [Bibliothèque YAML pour Arduino] (https://github.com/arduino-libraries/ArduinoYaml) : bibliothèque officielle pour utiliser YAML avec Arduino.
- [Documentation Arduino] (https://www.arduino.cc/reference/en/) : pour plus d'informations sur les différentes fonctions et bibliothèques.
- [Site officiel de YAML] (https://yaml.org/) : pour en savoir plus sur ce format de données.