---
title:                "Arduino: Envoyer une demande http avec une authentification de base"
simple_title:         "Envoyer une demande http avec une authentification de base"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un passionné d'Arduino et que vous souhaitez intégrer la communication avec des serveurs Web dans vos projets, alors savoir comment envoyer une requête HTTP avec une authentification de base peut être très utile. Cela vous permettra d'envoyer des données à distance et de les récupérer depuis un serveur, ce qui peut être très pratique pour des projets tels que la domotique ou l'IoT.

## Comment faire

Pour envoyer une requête HTTP avec une authentification de base en utilisant votre carte Arduino, vous aurez besoin de quelques composants supplémentaires :

- Une carte WiFi ou Ethernet pour communiquer avec le serveur
- Une bibliothèque HTTP pour Arduino comme [arduinoHTTPClient](https://github.com/arduino-libraries/ArduinoHttpClient)
- Un compte sur un serveur Web avec un nom d'utilisateur et un mot de passe pour l'authentification

Une fois que vous avez tous ces éléments, vous pouvez suivre les étapes suivantes :

1. Inclure la bibliothèque HTTP dans votre code en utilisant la directive ```#include <HttpClient.h>```
2. Déclarer un objet HTTPClient et une variable pour stocker la réponse ```HTTPClient http;```
3. Utiliser la méthode ```.begin(url)``` pour spécifier l'URL du serveur que vous souhaitez contacter avec votre requête.
4. Appeler la fonction ```.setAuthorization(user, password)``` en utilisant vos informations d'authentification pour configurer l'authentification de base.
5. Utiliser la méthode ```.get()``` pour envoyer la requête au serveur et stocker la réponse dans votre variable.
6. Utiliser les méthodes ```.statusCode()``` et ```.body()``` pour obtenir le code d'état de la réponse et les données renvoyées par le serveur.
7. Utiliser les méthodes ```.close()``` et ```.end()``` pour fermer la connexion et libérer la mémoire.

Voici un exemple de code pour envoyer une requête GET avec une authentification de base à un serveur :

```Arduino
#include <HttpClient.h>

HTTPClient http; // Déclarer l'objet HTTPClient
String url = "http://www.example.com/endpoint"; // Remplacer l'URL par celle de votre serveur
String user = "mon_nom"; // Remplacer par votre nom d'utilisateur
String pass = "mon_mot_de_passe"; // Remplacer par votre mot de passe
String response; // Variable pour stocker la réponse

void setup(){
    Serial.begin(9600); // Initialisation de la communication série
    http.begin(url); // Spécifier l'URL du serveur
    http.setAuthorization(user, pass); // Configurer l'authentification de base
}

void loop(){
    http.get(); // Envoyer la requête GET
    Serial.println(http.statusCode()); // Afficher le code d'état de la réponse
    response = http.body(); // Stocker les données renvoyées par le serveur
    Serial.println(response); // Afficher la réponse
    http.end(); // Fermer la connexion
    delay(5000); // Attendre quelques secondes avant d'envoyer une nouvelle requête
}
```

Voici un exemple de sortie série lorsque le serveur renvoie "Hello world!" en réponse à la requête :

```
200 //Code d'état
Hello world! //Données renvoyées par le serveur
```

## Plongée en profondeur

Maintenant que vous savez comment envoyer une requête HTTP avec une authentification de base, voici quelques informations supplémentaires qui peuvent être utiles :

- Les requêtes HTTP peuvent également utiliser l'authentification Digest, qui utilise un hachage pour sécuriser les informations d'authentification. Si votre serveur utilise l'authentification Digest, vous devrez utiliser la méthode ```.setDigestAuth(user, password)``` à la place de la méthode ```.setAuthorization()```.
- Les informations d'authentification peuvent également être passées directement dans l'URL en les spécifiant comme suit : ```http://user:password@server_url```.
- Si votre serveur nécessite un certificat SSL pour établir une connexion sécurisée, vous devrez utiliser la méthode ```.setCACert(cert)``` pour spécifier le certificat avant d'envoyer la requête.

Il est également important de noter que l'envoi de requêtes HTTP avec une authentification de base peut être moins sécurisé que les autres