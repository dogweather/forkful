---
title:                "Mise en majuscule d'une chaîne de caractères"
html_title:           "Arduino: Mise en majuscule d'une chaîne de caractères"
simple_title:         "Mise en majuscule d'une chaîne de caractères"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Capitaliser une chaîne signifie transformer toutes ses lettres en majuscules. Les programmeurs le font pour uniformiser les données textuelles, notamment pour les comparaisons ou affichages.

## Comment faire :

```Arduino
void setup() {
  Serial.begin(9600);
  String maChaine = "bonjour, Arduino!";
  maChaine.toUpperCase();
  Serial.println(maChaine);
}

void loop() {
  // Rien à faire ici.
}
```

Sortie attendue : `BONJOUR, ARDUINO!`

## Plongée en profondeur

Historiquement, les opérations sur les chaînes de caractères reflètent les besoins en traitement de texte des premiers jours de l'informatique. Capitaliser une chaîne est une opération basique mais essentielle. En Arduino, la méthode `toUpperCase()` est tout ce dont vous avez besoin pour transformer une chaîne de minuscules en majuscules. Cette fonction fait partie de la classe `String`, qui fournit plusieurs méthodes pour manipuler des textes. Les alternatives incluent la manipulation manuelle des caractères en utilisant leur code ASCII : 'a' à 'z' (97 à 122) peut être converti en 'A' à 'Z' (65 à 90) en soustrayant 32.

## Voir Aussi

- Documentation Arduino sur les chaînes de caractères: [Arduino String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Guide sur l'ASCII en Arduino : [Arduino ASCII Guide](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
