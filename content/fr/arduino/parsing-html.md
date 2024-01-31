---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:30:05.528819-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Le parsing HTML, c'est lire et décortiquer le code HTML pour en extraire des infos. Les programmeurs le font pour interagir avec des pages web depuis leurs applications, récupérer des données ou automatiser des tâches sur internet.

## Comment faire :

Le parsing HTML sur Arduino est un peu complexe parce qu'on n'a pas accès à des librairies puissantes comme sur un PC. Mais, voici un exemple basique :

```Arduino
#include <Ethernet.h>
#include <SPI.h>

// Initialisation du client Ethernet
EthernetClient client;

void setup() {
  // Démarrage de la connexion Ethernet et du port série
  Ethernet.begin(/* Vos configurations réseau ici */);
  Serial.begin(9600);

  // Connexion au serveur web
  if (client.connect("exemple.com", 80)) {
    client.println("GET /page.html HTTP/1.1");
    client.println("Host: exemple.com");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  // Attend que les données soient disponibles
  if (client.available()) {
    String line = client.readStringUntil('\n');
    // Ici le parsing simplifié, cherchant une balise spécifique
    if (line.indexOf("<title>") >= 0) {
      int start = line.indexOf("<title>") + 7; 
      int end = line.indexOf("</title>");
      Serial.println(line.substring(start, end));
    }
  }

  // Si la connexion est terminée, fermez-la
  if (!client.connected()) {
    Serial.println("Déconnexion");
    client.stop();
    while (true);
  }
}
```

Avec ce code, vous devriez voir le titre de la page HTML s'afficher sur votre moniteur série – et rien d'autre.

## Plongeon profond:

Historiquement, les Arduino n'étaient pas vraiment conçus pour le parsing HTML, vu leurs ressources limitées. Armer votre Arduino d'une connexion Ethernet ou WiFi, c'est déjà un gros pas. Pour un vrai parsing, sur un PC, vous utiliseriez des librairies comme BeautifulSoup en Python, bien plus puissantes.

Il existe des alternatives comme l'utilisation de services web externes qui prétraitent le HTML et renvoient des données plus digestes pour un microcontrôleur. Ou encore, coder un serveur intermédiaire personnalisé.

En terme de détails d'implémentation, sur Arduino, il faut garder ça simple. Pas d'arbre DOM ici, mais une recherche de chaînes de caractère qui fonctionne pour des besoins spécifiques et légers.

## Voir également :

- Documentation sur la librairie Ethernet d'Arduino : https://www.arduino.cc/en/reference/ethernet
- Un guide sur les requêtes HTTP avec Arduino : https://www.arduino.cc/en/Tutorial/LibraryExamples/WebClient
- Une discussion sur le parsing HTML avec Arduino sur les forums Arduino : http://forum.arduino.cc/index.php?topic=73816.0
