---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Une requête HTTP avec authentification de base est une technique pour access à des ressources web contrôlées en fournissant un nom d'utilisateur et un mot de passe en clair. Les programmeurs l'utilisent parce que c'est simple et largement accepté, malgré des problèmes évidents de sécurité.

## Comment faire :

Utiliser `ESP8266WiFiBasic` et `ESP8266HTTPClient` bibliothèques :
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "votre_ssid";
const char* password = "votre_password";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connexion...");
  }

  HTTPClient http;
  
  http.begin("http://example.com");
  http.setAuthorization("username", "password");
  
  int httpCode = http.GET();

  if(httpCode > 0) {
    String payload = http.getString();
    Serial.println(payload);
  } else {
    Serial.println("Erreur http...");
  }

  http.end();
}

void loop() {
}
```

## Plongeon Profond

L'authentification de base HTTP est existé depuis les premiers jours de l'internet. Mais à cause de sa vulnérabilité (transmettant un nom d'utilisateur et un mot de passe en clair), il n'est plus tout aussi populaire malgré la simplicité de son exécution.

Alternativement, vous pouvez utiliser l'authentification Digest, OAuth, ou même un token JWT. Chacun a ses avantages et inconvénients, selon l'application.

En envoyant une requête HTTP à l'aide d'Arduino, la gestion des connexions est crucial. Par exemple, dans notre code précédent, nous avons utilisé `http.end();` pour fermer la connexion HTTP après l'utilisation.

## Voir aussi :

2. Guide de l'ESP8266 WiFiBasic : [https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
3. Article de wikipedia sur l'authentification HTTP : [https://fr.wikipedia.org/wiki/Authentification_HTTP](https://fr.wikipedia.org/wiki/Authentification_HTTP)