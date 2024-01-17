---
title:                "Travailler avec le langage json"
html_title:           "Arduino: Travailler avec le langage json"
simple_title:         "Travailler avec le langage json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/working-with-json.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi est-ce important ?

JSON (JavaScript Object Notation) est un format de données couramment utilisé en programmation pour stocker et échanger des informations structurées. Les programmeurs utilisent JSON car il est simple et facile à lire et à écrire pour les ordinateurs, et il est également largement pris en charge par les différents langages de programmation.

## Comment faire :

Pour travailler avec du JSON dans vos projets Arduino, vous devrez utiliser des bibliothèques telles que ArduinoJson ou ESP8266JSON pour convertir les données au format JSON en variables que le microcontrôleur Arduino peut traiter. Vous pouvez ensuite utiliser ces variables pour effectuer différentes tâches, telles que lire des données depuis un serveur Web ou les enregistrer sur une carte SD.

```Arduino
#include <ArduinoJson.h> // Inclure la bibliothèque nécessaire
#include <ESP8266WiFi.h>

void setup() {
  Serial.begin(9600);
  
  // Se connecter à un réseau WiFi
  WiFi.begin("Nom_du_Réseau_WiFi", "Mot_de_passe_du_Réseau_WiFi");
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Connexion en cours...");
  }

  // Récupérer des données depuis une URL au format JSON
  HTTPClient http;
  http.begin("http://www.example.com/data.json");
  int httpCode = http.GET();
  
  if (httpCode == HTTP_CODE_OK) {
    // Convertir les données JSON en une variable
    String response = http.getString();
    DynamicJsonDocument doc(1024); // Créer un document JSON de 1024 octets
    deserializeJson(doc, response); // Parcourir le JSON et le stocker dans le document
    // Lire une valeur spécifique dans le document JSON
    int temperature = doc["temperature"];
    Serial.println("La température est de: " + String(temperature) + " degrés Celsius");
  }
}

void loop() {
  // Code de votre boucle principale
}
```

## Plongée profonde :

JSON a été créé en 2001 par Douglas Crockford et est basé sur la syntaxe de JavaScript. Il est souvent utilisé comme alternative au XML pour stocker et transférer des données en raison de sa lisibilité et de sa taille de fichier plus petite. En plus de l'utilisation d'Arduino, JSON est également largement utilisé dans les applications Web et mobiles.

Il existe plusieurs alternatives à JSON, telles que YAML, XML et CSV. Chaque format a ses propres avantages et inconvénients, et le choix dépendra souvent du type de données que vous devez manipuler.

Pour implémenter des fonctions de traitement JSON plus avancées, vous pouvez également utiliser des bibliothèques telles que ArduinoJson Assistant ou JSON Streaming Parser.

## À lire également :

- Tutoriel sur la manipulation de données JSON avec Arduino : https://randomnerdtutorials.com/decoding-and-encoding-json-with-arduino-or-esp8266/
- Détails sur la structure et la syntaxe JSON : https://www.json.org/
- Comparaison entre JSON et XML : https://stackoverflow.com/questions/4862310/json-vs-xml-which-one-is-better-for-what