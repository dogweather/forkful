---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:59.894030-07:00
description: "YAML (YAML Ain't Markup Language) est un standard de s\xE9rialisation\
  \ de donn\xE9es lisible par l'humain qui peut \xEAtre utilis\xE9 pour les fichiers\
  \ de\u2026"
lastmod: 2024-02-19 22:05:16.812813
model: gpt-4-0125-preview
summary: "YAML (YAML Ain't Markup Language) est un standard de s\xE9rialisation de\
  \ donn\xE9es lisible par l'humain qui peut \xEAtre utilis\xE9 pour les fichiers\
  \ de\u2026"
title: Travailler avec YAML
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

YAML (YAML Ain't Markup Language) est un standard de sérialisation de données lisible par l'humain qui peut être utilisé pour les fichiers de configuration, la communication entre programmes et le stockage de données. Les programmeurs se tournent vers YAML pour les projets Arduino afin de simplifier le processus de configuration de leurs applications, rendant plus facile la modification des paramètres sans plonger profondément dans le code, améliorant la lisibilité et facilitant le partage de configurations.

## Comment faire :

Travailler directement avec YAML sur Arduino n'est pas aussi simple que dans des environnements de programmation de plus haut niveau en raison des contraintes de mémoire et de l'absence de bibliothèques de traitement YAML natives. Cependant, pour les projets nécessitant l'analyse ou la génération de YAML, une approche typique implique l'utilisation d'un ordinateur compagnon (comme un Raspberry Pi) ou la conversion de fichiers YAML en un format plus adapté à Arduino (comme JSON) à l'aide de scripts externes. À des fins de démonstration, concentrons-nous sur cette dernière approche en utilisant une bibliothèque populaire : ArduinoJson.

**Étape 1 :** Convertissez votre configuration YAML en JSON. Vous pouvez utiliser des outils en ligne ou des utilitaires en ligne de commande comme `yq`.

Fichier YAML (`config.yaml`) :
```yaml
wifi:
  ssid: "VotreSSID"
  password: "VotreMotDePasse"
```

Converti en JSON (`config.json`) :
```json
{
  "wifi": {
    "ssid": "VotreSSID",
    "password": "VotreMotDePasse"
  }
}
```

**Étape 2 :** Utilisez la bibliothèque ArduinoJson pour analyser le fichier JSON dans votre croquis Arduino. Premièrement, vous devez installer la bibliothèque ArduinoJson via le gestionnaire de bibliothèques dans l'IDE Arduino.

**Étape 3 :** Chargez et analysez le JSON dans votre code. En raison des limitations de stockage d'Arduino, imaginez que la chaîne JSON est stockée dans une variable ou lue à partir d'une carte SD.

Exemple de croquis Arduino :
```cpp
#include <ArduinoJson.h>

const char* jsonConfig = "{\"wifi\":{\"ssid\":\"VotreSSID\",\"password\":\"VotreMotDePasse\"}}";

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, jsonConfig);

  if (error) {
    Serial.print(F("deserializeJson() a échoué : "));
    Serial.println(error.f_str());
    return;
  }

  const char* ssid = doc["wifi"]["ssid"]; // "VotreSSID"
  const char* password = doc["wifi"]["password"]; // "VotreMotDePasse"

  Serial.print("SSID : ");
  Serial.println(ssid);
  Serial.print("Mot de passe : ");
  Serial.println(password);
}

void loop() {
  // Rien ici pour cet exemple
}
```

Sortie lors de l'exécution du croquis :
```
SSID : VotreSSID
Mot de passe : VotreMotDePasse
```

Cette approche, impliquant la conversion en JSON et l'exploitation de la bibliothèque ArduinoJson, permet une gestion réalisable de la configuration YAML au sein des projets Arduino, contournant l'analyse directe de YAML sur le microcontrôleur.
