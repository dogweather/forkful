---
title:    "Arduino: Sortie de débogage par impression"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

**Pourquoi:** L'impression de sortie de débogage peut être un outil précieux pour les programmeurs Arduino. Cela permet de surveiller le fonctionnement de votre code et de détecter d'éventuelles erreurs ou problèmes de manière plus efficace.

**Comment faire:** Pour imprimer la sortie de débogage dans votre code Arduino, il suffit d'utiliser la fonction ```Serial.print ()```. Par exemple, si vous souhaitez imprimer une valeur entière, vous pouvez utiliser ```Serial.print (valeurEntière);```. Vous pouvez également ajouter du texte en utilisant ```Serial.print ("Texte ici");```. Vous pouvez ensuite surveiller la sortie de débogage en ouvrant le moniteur série dans l'IDE Arduino.

**Deep Dive:** Il existe plusieurs manières d'utiliser la fonction ```Serial.print ()``` pour un débogage précis. Vous pouvez utiliser ```Serial.println ()``` pour afficher une nouvelle ligne de texte après chaque instruction, ce qui rendra la sortie plus lisible. Vous pouvez également imprimer des valeurs sous forme binaire en utilisant ```Serial.print (valeur, BIN);```. De plus, vous pouvez utiliser la fonction ```Serial.write ()``` pour transmettre des données binaires brut à des périphériques externes.

**Voir aussi:** 
- Tutoriel sur l'impression de sortie de débogage en Arduino: [lien1](https://www.arduino.cc/en/Tutorial/SerialDebug)
- Documentation officielle sur la fonction ```Serial.print ()```: [lien2](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- Exemples de codes pour un débogage efficace: [lien3](https://blog.codingjunkie.net/arduino-serial-print-examples/)

N'hésitez pas à utiliser l'impression de sortie de débogage dans vos projets Arduino pour faciliter le processus de débogage. Avec ces connaissances de base et ces ressources supplémentaires, vous pourrez facilement détecter et résoudre tout problème dans votre code. Bon codage !