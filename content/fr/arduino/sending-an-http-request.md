---
title:                "Envoyer une requête http"
html_title:           "Arduino: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et Pourquoi?

Envoyer une requête HTTP, c'est simplement envoyer une demande à un serveur pour obtenir des informations ou pour effectuer une action. Les programmeurs le font souvent pour communiquer avec des services web tels que des API ou pour contrôler des appareils connectés à internet.

# Comment faire:

Voici comment envoyer une requête HTTP en utilisant Arduino:

```Arduino
#include <WiFi.h>
#include <WiFiClient.h>
#include <HTTPClient.h>

const char *ssid = "MonWifi"; // Remplacez par le nom de votre réseau WiFi
const char *password = "MotDePasse"; // Remplacez par votre mot de passe WiFi

void setup()
{
    Serial.begin(115200); // Démarre la communication série
    WiFi.begin(ssid, password); // Se connecte au réseau WiFi
    Serial.println("En attente de connexion WiFi...");
    
    while (WiFi.status() != WL_CONNECTED) { // Attendez une connexion WiFi
        delay(500);
        Serial.print(".");
    }
    
    Serial.println("Connecté au WiFi");
}

void loop()
{
    WiFiClient client; // Instancie un client WiFi
    HTTPClient http; // Instancie un client HTTP
    
    Serial.print("Envoi d'une requête a l'adresse: ");
    Serial.println("httpbin.org/ip");
    
    http.begin(client, "http://httpbin.org/ip"); // Débute une requête HTTP vers httpbin.org
    int httpResponseCode = http.GET(); // Envoie la requête GET
    
    if (httpResponseCode > 0) {
        String response = http.getString(); // Obtient la réponse
        
        Serial.println(httpResponseCode); // Affiche le code de réponse
        Serial.println(response); // Affiche la réponse
    }
    else {
        Serial.println("Erreur lors de la requête");
    }
    
    http.end(); // Termine la requête HTTP
    
    delay(5000); // Attend 5 secondes avant de recommencer
}
```

Lorsque vous exécutez ce code, le moniteur série affichera le code de réponse de la requête ainsi que la réponse du serveur. Dans cet exemple, le code de réponse devrait être 200 et la réponse devrait contenir votre adresse IP.

# Plongée en profondeur:

Envoyer des requêtes HTTP est une pratique courante dans le développement web, car cela permet de communiquer avec des services externes et de récupérer des données en temps réel. Alternativement, vous pouvez également utiliser des protocoles tels que MQTT ou UDP pour la communication entre appareils connectés. L'envoi de requêtes HTTP avec Arduino peut également être réalisé en utilisant des bibliothèques tierces telles que HttpClient ou cURL.

# Voir aussi:

Pour en savoir plus sur l'envoi de requêtes HTTP avec Arduino, vous pouvez consulter les ressources suivantes:

- Développement WiFi avec Arduino: https://www.arduino.cc/en/Reference/WiFi
- Cours de base sur l'HTTP: https://www.tutorialspoint.com/http/index.htm
- Bibliothèque HttpClient: https://github.com/amcewen/HttpClient