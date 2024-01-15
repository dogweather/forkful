---
title:                "Utilisation des expressions régulières"
html_title:           "Arduino: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Avant de plonger dans l'utilisation des expressions régulières en Arduino, vous devez comprendre leur utilité. Les expressions régulières sont un moyen pratique et efficace de trouver, filtrer et manipuler des données dans du texte. Elles sont particulièrement utiles pour les projets d'Arduino qui impliquent la manipulation de données en texte, comme la communication avec un capteur ou l'affichage de données sur un écran.

## Comment faire

Les expressions régulières en Arduino sont implémentées via la bibliothèque `Regexp`. Voici un exemple de code pour trouver et afficher tous les chiffres dans une chaîne de caractères :

```arduino
#include <Regexp.h>

// Chaîne de caractères de test
String texte = "Il y a 123 pingouins dans le zoo";

void setup() {
  Serial.begin(9600);
}

void loop() {
  // Créer un objet Regexp pour trouver les chiffres
  Regexp chiffres("[0-9]+");

  // Chercher les correspondances dans la chaîne de caractères
  String resultat = chiffres.find(texte);

  // Afficher les résultats
  Serial.println(resultat);
}
```

Résultat :

```
123
```

Ce n'est qu'un exemple simple, mais les expressions régulières peuvent être utilisées pour effectuer des recherches bien plus complexes comme la validation de formats d'adresses email ou de numéros de téléphone.

## Plongée en profondeur

Les expressions régulières peuvent sembler intimidantes au début, mais une fois que vous aurez compris leur syntaxe, elles deviendront un outil extrêmement utile pour manipuler des données en texte. Voici quelques astuces pour vous aider à plonger en profondeur :

- Les crochets `[ ]` sont utilisés pour définir un ensemble de caractères. Par exemple, `[aeiou]` correspond à n'importe quelle voyelle. Vous pouvez également utiliser un tiret `-` pour spécifier une plage de caractères, par exemple `[a-z]` correspond à n'importe quelle lettre minuscule.
- L'astérisque `*` correspond à zéro, un ou plusieurs occurrences du caractère précédent. Par exemple, `a*` va correspondre à `a`, `aa`, `aaa`, etc.
- Le point `.` correspond à n'importe quel caractère.
- Les parenthèses `()` sont utilisées pour grouper des expressions régulières. Par exemple, `\(.*\)` correspondra à n'importe quelle expression entre parenthèses.

Il existe de nombreuses ressources en ligne pour vous aider à apprendre et à maîtriser les expressions régulières. N'hésitez pas à les consulter pour en savoir plus sur cette puissante fonctionnalité.

## Voir aussi

- [Documentation de la bibliothèque Regexp pour Arduino](https://www.arduino.cc/reference/en/libraries/regexp/)
- [Guide complet sur les expressions régulières](https://www.regular-expressions.info/)
- [Démo en ligne pour tester vos expressions régulières](https://regexr.com/)