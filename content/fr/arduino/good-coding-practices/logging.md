---
date: 2024-01-26 00:59:04.557566-07:00
description: "Comment faire : L'Arduino ne dispose pas d'une librairie de journalisation\
  \ int\xE9gr\xE9e comme certains autres environnements, mais vous pouvez mettre en\
  \ \u0153uvre\u2026"
lastmod: '2024-03-13T22:44:58.119323-06:00'
model: gpt-4-1106-preview
summary: "L'Arduino ne dispose pas d'une librairie de journalisation int\xE9gr\xE9\
  e comme certains autres environnements, mais vous pouvez mettre en \u0153uvre une\
  \ journalisation de base vers la console S\xE9rie avec peu d'efforts."
title: Journalisation
weight: 17
---

## Comment faire :
L'Arduino ne dispose pas d'une librairie de journalisation intégrée comme certains autres environnements, mais vous pouvez mettre en œuvre une journalisation de base vers la console Série avec peu d'efforts. Voici un exemple rapide pour commencer :

```arduino
void setup() {
  // Commence la communication série avec le débit en bauds spécifié
  Serial.begin(9600);

  // Attendre que le port série se connecte - seulement nécessaire sur certaines cartes
  while (!Serial) {
    ; // attendre que le port série se connecte. Nécessaire pour l'USB natif
  }

  // Journaliser un message d'information indiquant que le processus de configuration est terminé
  Serial.println("Configuration terminée !");
}

void loop() {
  // Enregistreur simple qui affiche le temps de fonctionnement toutes les secondes
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Temps de fonctionnement (ms) : ");
    Serial.println(currentMillis);

    // Ici, vous pourriez également ajouter des journaux d'erreurs, des avertissements ou d'autres informations.
  }
  
  // Le reste de la logique de votre programme ici...
}
```

Exemple de sortie Série :
```
Configuration terminée !
Temps de fonctionnement (ms) : 1000
Temps de fonctionnement (ms) : 2000
Temps de fonctionnement (ms) : 3000
...
```

## Plongée en profondeur :
Historiquement, la journalisation sur les microcontrôleurs n'était pas aussi simple que sur un système d'exploitation complet. Les ressources limitées signifiaient que chaque octet comptait, et les développeurs devaient faire attention à ne pas engorger le système. Avec l’avènement de cartes plus capables et la simplification du processus par la plateforme Arduino, la journalisation est devenue plus accessible.

Bien que le code ci-dessus montre comment faire de la journalisation via l'interface Série, d’autres méthodes incluent l'écriture sur une carte SD, l'envoi de données sur un réseau à un serveur distant ou même l'affichage sur un petit écran LCD.

La mise en place d'un système de journalisation soulève des considérations telles que la rotation, le niveau de gravité des logs (info, debug, avertissement, erreur), et l'impact sur les performances. Sur un Arduino, vous devrez peut-être tenir compte des contraintes de mémoire lors de l’enregistrement de structures de données complexes. Pour la journalisation à distance, la sécurité des journaux transmis est également une préoccupation.

Des solutions plus sophistiquées comme Syslog, un standard de journalisation largement adopté, existent en dehors du monde Arduino, mais vous pouvez intégrer des bibliothèques tierces qui offrent des fonctionnalités similaires avec divers degrés de complexité et de besoins en ressources.

## Voir aussi :
- [Référence `Serial` Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Journalisation sur carte SD avec Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [Shield de journalisation de données de SparkFun](https://www.sparkfun.com/products/13712)
- [TinyWeb : un exemple pratique de journalisation à distance avec Arduino](https://www.arduino.cc/en/Tutorial/WebClientRepeating)
