---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Extraire des sous-chaînes consiste à prélever une partie spécifique d'une chaîne de caractères donner. C'est un outil majeur pour les programmeurs recherchant précision et flexibilité dans le traitement des données textuelles.

## Comment faire:

Je vais vous montrer comment faire cela avec Arduino. Prenons cette chaîne de caractères "Bonjour tout le monde!" et extrayons "tout le monde" d'elle.

```Arduino
String maChaine = "Bonjour tout le monde!";
String sousChaine = maChaine.substring(8, 20);
Serial.println(sousChaine);
```
Si vous exécutez cela, la sortie sera `tout le monde`.

## Plongée en profondeur

Sur le plan historique, le besoin d'extraire des sous-chaînes est presque aussi ancien que le codage. Développé pour traiter des situations où seules certaines parties d'un texte sont nécessaires, il demeure essentiel dans de nombreux contextes modernes comme l'analyse des données.

En termes d'alternatives, plusieurs autres langages de programmation offrent des fonctionnalités similaires, parfois avec des syntaxes différentes. Par exemple, en Python, vous utiliseriez la fonction slice tandis qu'en Java, vous utiliseriez également la méthode substring.

En ce qui concerne les détails de mise en œuvre, la méthode substring commence à extraire à l'index de début jusqu'à, mais sans inclure, l'index de fin. Et souvenez-vous que l'index commence à 0 en programmation.

## Voir aussi

Pour plus d'informations sur la manipulation de chaînes avec Arduino, consultez ces ressources:

1. La documentation officielle d'Arduino sur la classe String: [Lien](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
2. Une explication détaillée sur Instructables de l'utilisation des chaînes avec Arduino: [Lien](https://www.instructables.com/Arduino-String-Manipulation-Using-String-Functions/)
3. Un guide complet de MakeUseOf sur la programmation Arduio pour débutants: [Lien](https://www.makeuseof.com/tag/arduino-programming-for-beginners/)