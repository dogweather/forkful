---
title:                "Travailler avec json"
html_title:           "Arduino: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes-vous déjà demandé comment votre smartphone est en mesure de communiquer avec d'autres appareils sans fil et afficher les informations exactes? La réponse réside dans l'utilisation de JSON, un format de données très populaire pour l'échange de données. En apprenant à travailler avec JSON sur Arduino, vous pourriez créer des projets impressionnants qui peuvent communiquer avec d'autres appareils et récupérer des données précises.

## Comment faire

Pour commencer à travailler avec JSON sur Arduino, vous aurez besoin de deux bibliothèques: Arduino JSON et ArduinoHttpClient. La première sera utilisée pour analyser les données JSON et la seconde pour communiquer avec un serveur web. Après avoir installé ces bibliothèques, voici un exemple de code pour vous montrer comment récupérer et afficher les données JSON:

```Arduino
#include <ArduinoJson.h>
#include <ArduinoHttpClient.h>

HttpClient http;
http.begin("https://monserveurweb.com/donnees.json"); // Remplacez l'URL par celle de votre serveur web
int code = http.GET();

if(code > 0) { // Vérifie si la requête est réussie
    String data = http.getString(); // Stocke les données reçues dans une chaîne de caractères
    DynamicJsonBuffer jsonBuffer; // Crée un tampon de mémoire pour stocker les données
    JsonObject& root = jsonBuffer.parseObject(data); // Parse les données reçues en JSON
    const char* temperature = root["temperature"]; // Récupère la valeur de la clé "temperature"
    Serial.println(temperature); // Affiche la valeur dans le moniteur série
}
http.end(); // Termine la communication avec le serveur
```

Voici un exemple de données JSON que vous pourriez recevoir et afficher à partir de ce code:

```JSON
{
  "temperature": "25.5C",
  "humidite": "62%",
  "pression": "1018.2hPa"
}
```

En utilisant les bibliothèques et le code appropriés, vous pouvez récupérer différentes données de n'importe quel serveur web ou API et les utiliser dans vos projets Arduino.

## Plongée profonde

Maintenant que vous avez appris les bases de la récupération et du traitement des données JSON sur Arduino, il est temps de plonger plus en profondeur. Voici quelques points importants à retenir:

- N'oubliez pas de vérifier si la requête a réussi avant de commencer à analyser les données.
- Utilisez un tampon de mémoire pour stocker les données JSON et évitez les dépassements de mémoire.
- Les données JSON peuvent être de différents types (chaînes de caractères, nombres, tableaux, objets), assurez-vous d'utiliser les bonnes méthodes pour les analyser.
- Vous pouvez également créer des objets JSON à l'aide de la bibliothèque Arduino JSON et les envoyer à un serveur ou une API.

En apprenant les subtilités de la manipulation des données JSON, vous pourriez facilement étendre vos projets Arduino et leur ajouter des fonctionnalités de communication et de récupération de données en temps réel.

## Voir aussi

Maintenant que vous avez les bases pour travailler avec JSON sur Arduino, vous pourriez explorer davantage avec ces ressources:

- [Documentation officielle d'Arduino JSON](https://arduinojson.org/)
- [Documentation officielle d'ArduinoHttpClient](https://github.com/arduino-libraries/ArduinoHttpClient)
- [Ce tutoriel par Hackster](https://www.hackster.io/qozenaq/wifi-iot-temperature-sensor-with-json-data-on-arduino-ide-7c4794) qui détaille l'utilisation d'Arduino JSON pour envoyer des données à une page web via Wi-Fi. 

Maintenant, c'est à vous de jouer avec JSON et Arduino! Bonne chance!