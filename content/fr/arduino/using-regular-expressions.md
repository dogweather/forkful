---
title:                "Utiliser les expressions régulières"
html_title:           "C: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Les expressions régulières sont des séquences de caractères formulant un motif de recherche. Les programmeurs les utilisent pour rechercher, correspondre et manipuler des chaînes de caractères dans le texte.

## Comment:

Écrivez l'expression régulière entre des guillemets pour commencer.

```Arduino
String data = "Arduino 2.34"; 
Serial.println(data.matches("Arduino \\d\\.\\d\\d")); 
```

Cela renverra `true` si votre chaîne correspond au motif que vous avez spécifié. Le `\\d` signifie un chiffre, donc nous cherchons le mot "Arduino" suivi d'un espace, d'un chiffre, d'un point et de deux autres chiffres.

## Plongée en Profondeur

Historiquement, les expressions régulières ont des racines dans les mathématiques théoriques du XXe siècle. Conçues en 1956, elles sont devenues une caractéristique inévitable de nombreux langages de programmation. Cependant, Arduino n'a pas de support natif pour les expressions régulières, mais plusieurs bibliothèques tierces peuvent être utilisées.

En ce qui concerne les alternatives, vous pouvez utiliser la manipulation de chaînes classique, mais elle peut devenir complexe pour les motifs plus difficiles à gérer.

Concernant la mise en œuvre, la correspondance de motif implique souvent un backtracking, un processus récursif qui peut être coûteux en termes de performance, il est donc important d'utiliser judicieusement les expressions régulières.

## Voir Aussi

Pour plus d'informations sur la programmation Arduino avec les expressions régulières, vous pouvez consulter les sources suivantes :

- Site officiel d'Arduino : https://www.arduino.cc/
- Documentation des expressions régulières : https://regexone.com/
- Stack Overflow en Français : https://fr.stackoverflow.com/questions/tagged/arduino.