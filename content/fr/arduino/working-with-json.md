---
title:                "Arduino: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

La programmation avec Arduino est devenue très populaire ces dernières années et elle a ouvert de nombreuses possibilités pour les amateurs et les professionnels. Une de ces possibilités est l'utilisation du format de données JSON pour communiquer avec d'autres appareils et services en ligne. Apprendre à travailler avec JSON vous permettra d'étendre les fonctionnalités de vos projets Arduino et de les connecter à une variété d'applications.

## Comment faire

Pour commencer avec JSON, vous devez d'abord inclure la bibliothèque [ArduinoJSON](https://arduinojson.org/) dans votre code. Cette bibliothèque facilite la création, la manipulation et la lecture de données JSON en utilisant des méthodes simples. Voici un exemple de code pour créer un objet JSON avec deux paires de clés/valeurs :

```Arduino
#include <ArduinoJson.h>

void setup() {
    // Initialisation du buffer de 200 octets pour stocker les données
    StaticJsonBuffer<200> jsonBuffer;
    
    // Création de l'objet JSON avec deux paires de clés/valeurs
    JsonObject& data = jsonBuffer.createObject();
    data["nom"] = "Jean";
    data["age"] = 25;

    // Conversion de l'objet en chaîne de caractères pour l'afficher
    data.printTo(Serial);
}

void loop() {
    // Rien ici pour cet exemple
}
```

Lorsque vous téléversez ce code sur votre carte Arduino et ouvrez le moniteur série, vous devriez voir la chaîne de caractères suivante s'afficher : `{"nom":"Jean","age":25}`. Vous pouvez ensuite utiliser cette structure de données pour l'envoyer à un service en ligne ou la manipuler comme bon vous semble.

## Plongée en profondeur

Il existe plusieurs méthodes pour manipuler les données dans un objet JSON, telles que `set()` et `get()` pour modifier ou obtenir des valeurs spécifiques, `filter()` pour filtrer les données en fonction d'un critère, ou encore `prettyPrintTo()` pour un affichage plus lisible. N'hésitez pas à consulter la [documentation de la bibliothèque ArduinoJSON](https://arduinojson.org/v6/api/jsonobject/) pour découvrir toutes les possibilités.

De plus, si vous souhaitez recevoir des données JSON de services en ligne, vous devrez apprendre à utiliser les méthodes `parse()` ou `readObject()` pour les récupérer et les stocker dans un objet JSON.

## Voir aussi

* [Tutoriel ArduinoJSON](https://arduinojson.org/v6/doc/serialization/)
* [Exemple de projet Arduino utilisant JSON](https://create.arduino.cc/projecthub/royongpoom/how-to-communicate-between-two-arduinos-04bf34)
* [Vidéo explicative sur l'utilisation de JSON avec Arduino](https://www.youtube.com/watch?v=tdxqBFFegio)