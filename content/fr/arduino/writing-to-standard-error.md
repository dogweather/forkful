---
title:                "Écrire dans l'erreur standard"
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire sur l'erreur standard (stderr) permet de signaler des erreurs et des messages de diagnostic sans mêler ces infos aux données normales de sortie (stdout). Les développeurs l'utilisent pour déboguer et suivre le flux d'exécution du programme.

## How to:
Arduino ne supporte pas nativement stderr. On utilise `Serial` pour le débogage. Pour simuler stderr, on écrit sur Serial avec des tags d'erreur.
```Arduino
void setup() {
  Serial.begin(9600); // On initie la communication série
}

void loop() {
  // Simulation d'une erreur
  Serial.println("[ERREUR] Une erreur s'est produite.");
}
```
Résultat dans le moniteur série: `[ERREUR] Une erreur s'est produite.`

## Deep Dive
Historiquement, stderr est un concept Unix, détaché des fonctionnalités d'Arduino. Des alternatives comme des bibliothèques de journalisation sur SD existent. Pour implémenter une sortie d'erreur en Arduino, on pourrait rediriger le flux Serial vers un fichier ou une interface différente si cela est nécessaire.

## See Also
- Documentation Arduino sur `Serial`: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Article sur la redirection de sortie en C: https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)