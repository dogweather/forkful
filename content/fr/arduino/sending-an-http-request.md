---
title:                "Arduino: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous souhaitez envoyer des données à un serveur distant ou recevoir des informations à partir d'une API en ligne, vous aurez besoin d'envoyer une requête HTTP. Cela peut sembler intimidant au début, mais grâce à la puissance de l'Arduino et à quelques lignes de code, vous pourrez facilement communiquer avec des serveurs externes.

# Comment Faire

Pour envoyer une requête HTTP avec l'Arduino, vous aurez besoin d'une bibliothèque appelée "Ethernet". Vous pouvez l'installer en allant dans le menu "Sketch", puis en sélectionnant "Inclure une bibliothèque" et enfin "Gérer les bibliothèques". Recherchez "Ethernet" et cliquez sur "Installer".

Une fois la bibliothèque installée, vous pouvez commencer à écrire votre code. Tout d'abord, vous devrez inclure la bibliothèque Ethernet en ajoutant la ligne suivante au début de votre code :

```Arduino
#include <Ethernet.h>
```

Ensuite, vous devrez définir certaines variables telles que l'adresse IP de votre Arduino, l'adresse IP du serveur distant et le port de communication. Vous devrez également définir un objet de type "EthernetClient" pour établir la connexion.

Une fois que toutes ces variables sont définies, vous pouvez utiliser la fonction "client.connect()". Cette fonction prendra en paramètres l'adresse IP et le port du serveur et établira une connexion avec celui-ci. Ensuite, vous pourrez envoyer votre requête à l'aide de la fonction "client.println()". N'oubliez pas de terminer votre requête avec la ligne vide "client.println()".

Voici un exemple de code complet pour envoyer une requête HTTP à un serveur :

```Arduino
#include <Ethernet.h>

IPAddress serverIP(192, 168, 1, 1); // Adresse IP du serveur
int serverPort = 80; // Port de communication
EthernetClient client; // Objet de connexion

void setup() {
  Ethernet.begin(mac); // Définit l'adresse MAC de votre Arduino
  Serial.begin(9600); // Initialise la communication série
  delay(1000); // Attendez une seconde que l'Ethernet se connecte
}

void loop() {
  if (client.connect(serverIP, serverPort)) { // Connectez-vous au serveur
    // Envoyez votre requête
    client.println("GET /api/data HTTP/1.1");
    client.println("Host: 192.168.1.1");
    client.println("Connection: close");
    client.println();
  }

  while (client.available()) { // Attendez une réponse du serveur
    char c = client.read(); // Lire chaque caractère de la réponse
    Serial.print(c); // Imprimez-le sur le moniteur série
  }
  
  delay(5000); // Attendez 5 secondes avant d'envoyer une autre requête
}
```

Lorsque vous téléversez ce code sur votre Arduino, assurez-vous que votre ordinateur est connecté au même réseau que votre Arduino. Vous devriez voir une réponse du serveur dans le moniteur série.

# Plongée Profonde

Lorsque vous envoyez une requête HTTP, vous pouvez également inclure des en-têtes qui fournissent des informations supplémentaires au serveur. Par exemple, vous pourriez inclure un en-tête "Authorization" pour vous authentifier auprès du serveur.

De plus, vous pouvez également envoyer des données sous forme de paramètres dans votre requête, tels que des valeurs de capteurs ou des données d'utilisateurs.

Il existe également différentes méthodes de requête que vous pouvez utiliser, telles que "GET", "POST", "PUT" ou "DELETE", en fonction de ce que vous souhaitez faire avec le serveur.

N'hésitez pas à explorer les différentes possibilités pour envoyer des requêtes HTTP avec votre Arduino.

# Voir Aussi

- Tutoriel officiel sur l'utilisation de la bibliothèque Ethernet : https://www.arduino.cc/en/Reference/Ethernet
- Exemples de projets utilisant l'envoi de requêtes HTTP avec l'Arduino : https://create.arduino.cc/projecthub/projects/tags/http
- Documentation sur les méthodes de requête et les en-têtes HTTP : https://developer.mozilla.org/fr/docs/Web/HTTP/Methods et https://developer.mozilla.org/fr/docs/Web/HTTP/Headers