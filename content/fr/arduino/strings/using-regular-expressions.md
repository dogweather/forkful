---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:56.125232-07:00
description: "Comment : Arduino n\u2019a pas de support int\xE9gr\xE9 pour les regex\
  \ directement dans sa biblioth\xE8que standard. Cependant, vous pouvez obtenir une\
  \ fonctionnalit\xE9\u2026"
lastmod: '2024-03-13T22:44:58.098183-06:00'
model: gpt-4-0125-preview
summary: "Arduino n\u2019a pas de support int\xE9gr\xE9 pour les regex directement\
  \ dans sa biblioth\xE8que standard."
title: "Utilisation des expressions r\xE9guli\xE8res"
weight: 11
---

## Comment :
Arduino n’a pas de support intégré pour les regex directement dans sa bibliothèque standard. Cependant, vous pouvez obtenir une fonctionnalité similaire aux regex pour des motifs simples en utilisant des fonctions de chaînes de base, ou, pour des besoins plus complexes, intégrer une bibliothèque tierce telle que `regex`.

### Correspondance de Chaînes Basique sans Regex
Pour les besoins basiques, comme trouver une sous-chaîne, vous pouvez utiliser la fonction `String.indexOf()` :
```cpp
String data = "Valeur du capteur : 12345";
int index = data.indexOf("valeur :");
if (index != -1) {
  String valeur = data.substring(index + 7).trim();
  Serial.println(valeur); // Affiche : 12345
}
```

### Utiliser une Bibliothèque Tierce pour Regex
Pour gérer des motifs plus complexes, vous pourriez envisager une bibliothèque comme `regex`. Après avoir installé la bibliothèque, vous pouvez l’utiliser comme suit :

1. **Installation** : La bibliothèque `regex` pourrait ne pas être directement disponible dans le Gestionnaire de Bibliothèques Arduino, donc vous pourriez avoir besoin de l'installer manuellement en la téléchargeant depuis une source fiable et en l'ajoutant à votre dossier de bibliothèques Arduino.

2. **Exemple d'Utilisation** :
En supposant que la bibliothèque offre des fonctionnalités similaires aux implémentations regex standard, vous pourriez l'utiliser comme suit :

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // Attend que Serial soit prêt
  
  regex_t reg;
  const char* motif = "[0-9]+"; // Correspond à une séquence de chiffres
  regcomp(&reg, motif, REG_EXTENDED);
  
  const char* test_str = "Valeur du capteur : 12345";
  
  regmatch_t correspondances[1];
  if (regexec(&reg, test_str, 1, correspondances, 0) == 0) {
    // Extraire et imprimer la partie correspondante
    int debut = correspondances[0].rm_so;
    int fin = correspondances[0].rm_eo;
    char correspondance[fin-debut+1];
    strncpy(correspondance, test_str + debut, fin-debut);
    correspondance[fin-debut] = '\0';
    
    Serial.print("Correspondance trouvée : ");
    Serial.println(correspondance); // Affiche : 12345
  } else {
    Serial.println("Aucune correspondance trouvée");
  }
  
  regfree(&reg); // Libère la mémoire allouée pour regex
}

void loop() {
  // mettez ici votre code principal, pour s'exécuter de manière répétée :
}
```

**Note** : La syntaxe et les fonctions spécifiques utilisées ici sont à des fins illustratives et peuvent varier en fonction des détails de mise en œuvre réels de la bibliothèque `regex` que vous choisissez. Reportez-vous toujours à la documentation de la bibliothèque pour obtenir des informations précises et à jour.
