---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:36.275118-07:00
description: "Comment faire : Arduino ne supporte pas nativement des op\xE9rations\
  \ complexes sur les syst\xE8mes de fichiers directement. Cependant, gr\xE2ce \xE0\
  \ l'utilisation de\u2026"
lastmod: '2024-03-13T22:44:58.129537-06:00'
model: gpt-4-0125-preview
summary: "Arduino ne supporte pas nativement des op\xE9rations complexes sur les syst\xE8\
  mes de fichiers directement."
title: "V\xE9rifier si un r\xE9pertoire existe"
weight: 20
---

## Comment faire :
Arduino ne supporte pas nativement des opérations complexes sur les systèmes de fichiers directement. Cependant, grâce à l'utilisation de la bibliothèque SD, qui fait partie de l'environnement de développement standard Arduino IDE, vous pouvez facilement travailler avec des fichiers et des répertoires. Pour vérifier si un répertoire existe, vous devez d'abord initialiser la carte SD puis utiliser la méthode `exists()` de la bibliothèque SD.

Premièrement, incluez la bibliothèque SD et déclarez la broche de sélection de puce :

```cpp
#include <SPI.h>
#include <SD.h>

const int chipSelect = 4; // Broche de sélection de puce pour le module de carte SD
```

Dans votre fonction `setup()`, initialisez la carte SD et vérifiez si le répertoire existe :

```cpp
void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(chipSelect)) {
    Serial.println("L'initialisation a échoué !");
    return;
  }

  // Vérifiez si le répertoire existe
  if (SD.exists("/myDir")) {
    Serial.println("Le répertoire existe.");
  } else {
    Serial.println("Le répertoire n'existe pas.");
  }
}
```
Dans la fonction `loop()`, vous pouvez la laisser vide ou ajouter d'autres codes opérationnels selon le besoin :

```cpp
void loop() {
  // Code opérationnel ou gardé vide
}
```

Le résultat de l'exécution du code sera soit :

```
Le répertoire existe.
```
ou

```
Le répertoire n'existe pas.
```

Il est important de s'assurer que la carte SD est correctement formatée et que le chemin du répertoire `/myDir` correspond à vos besoins spécifiques. Cette vérification basique est une pierre angulaire pour effectuer des opérations plus complexes avec des fichiers et des répertoires sur des cartes SD avec Arduino.
