---
title:                "Retirer les guillemets d'une chaîne"
aliases:
- fr/arduino/removing-quotes-from-a-string.md
date:                  2024-01-26T03:37:07.351486-07:00
model:                 gpt-4-0125-preview
simple_title:         "Retirer les guillemets d'une chaîne"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Supprimer les guillemets d'une chaîne signifie éliminer toutes les instances de caractères de guillemets simples (`'`) ou doubles (`"`) qui entourent le texte. Les programmeurs font souvent cela pour assainir les entrées, préparer les chaînes pour la comparaison, ou traiter des données textuelles qui pourraient accidentellement inclure des guillemets comme partie du contenu de la chaîne.

## Comment faire :
Pour supprimer les guillemets d'une chaîne dans Arduino, vous pouvez parcourir les caractères et reconstruire la chaîne sans les caractères de guillemet. Par exemple :

```arduino
String removeQuotes(String str) {
  String result = ""; // Créer une chaîne vide pour tenir le résultat
  for (int i = 0; i < str.length(); i++) {
    if (str[i] != '"' && str[i] != '\'') { // Vérifier chaque caractère
      result += str[i]; // Ajouter au résultat s'il ne s'agit pas d'un guillemet
    }
  }
  return result;
}

void setup() {
  Serial.begin(9600);
  String testStr = "'Bonjour, monde !'";
  Serial.println(removeQuotes(testStr)); // Devrait imprimer : Bonjour, monde !
}

void loop() {
  // Rien à faire ici
}
```

Exemple de sortie sur le moniteur série serait :
```
Bonjour, monde !
```

## Exploration Approfondie
Le concept de suppression de caractères d'une chaîne n'est pas unique à Arduino ; il est courant dans de nombreux environnements de programmation. Historiquement, les fonctions de manipulation de chaîne ont été une partie essentielle des langages de programmation pour permettre aux développeurs de nettoyer et d'analyser les données efficacement.

En plus de boucler manuellement et de construire une nouvelle chaîne comme indiqué ci-dessus, il existe des méthodes alternatives. Par exemple, on pourrait utiliser la méthode `replace()` pour substituer les guillemets par une chaîne vide, bien qu'il y ait des compromis en termes de lisibilité et de gestion des caractères d'échappement.

```arduino
String removeQuotes(String str) {
  str.replace("\"", ""); // Remplace tous les guillemets doubles
  str.replace("\'", ""); // Remplace tous les guillemets simples
  return str;
}
```

Comprendre les compromis est vital. La méthode de boucle peut être plus lente pour les longues chaînes mais est explicite et facile à personnaliser (comme si vous aviez besoin de supprimer uniquement les guillemets de début et de fin). La méthode `replace()` est plus concise et généralement plus rapide, mais cela devient plus délicat s'il faut gérer les caractères de guillemet échappés à l'intérieur de la chaîne.

## Voir Aussi
- Référence de chaîne Arduino : https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Guide de W3Schools sur la manipulation de chaîne C++ (lié au langage d'Arduino) : https://www.w3schools.com/cpp/cpp_strings.asp
- Discussions sur Stack Overflow concernant la manipulation de chaîne en C++ (langage de base d'Arduino) : https://stackoverflow.com/questions/tagged/string+cpp
