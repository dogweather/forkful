---
title:    "Arduino: Utiliser des expressions régulières"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes un programmeur Arduino, vous connaissez probablement l'importance de la manipulation de données. Mais saviez-vous qu'il existe un outil puissant pour vous aider à traiter les données plus efficacement? Les expressions régulières, ou "regex", sont un moyen efficace de rechercher et de manipuler des données en utilisant des motifs spécifiques. Dans cet article, nous allons explorer pourquoi les regex sont utiles pour les programmeurs Arduino.

## Comment faire
Les expressions régulières utilisent des motifs pour correspondre à des chaînes de caractères spécifiques. Elles peuvent être utilisées pour vérifier si une chaîne correspond à un motif donné ou pour extraire des informations d'une chaîne de caractères. Voyons quelques exemples pratiques en utilisant des __codes Arduino__.

- Vérifier si une adresse e-mail est valide:

```Arduino
String email = "john@example.com";

// Recherche d'un motif correspondant à une adresse e-mail valide
bool isValid = regexMatch(email, "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+.[A-Za-z]{2,4}$");

if (isValid) {
  Serial.println("Cette adresse e-mail est valide!");
} else {
  Serial.println("Cette adresse e-mail n'est pas valide!");
}
```

- Extraire le code postal à partir d'une adresse:

```Arduino
String address = "123 Main Street, Seattle, WA 98101";

// Recherche d'un motif correspondant à un code postal
reMatchResult result = regexExec(address, "[0-9]{5}");

Serial.print("Le code postal est: ");
Serial.println(result.match);
```

En utilisant les expressions régulières, vous pouvez effectuer des tâches complexes de manipulation de données en quelques lignes de code seulement. N'hésitez pas à expérimenter avec différents motifs pour voir leurs effets sur vos données.

## Plongée en profondeur
Si vous êtes curieux d'en savoir plus sur les expressions régulières, il y a plusieurs concepts clés à comprendre:

- Les caractères spéciaux, comme `$`, `+` et `{}`, ont des significations spécifiques en regex et doivent être échappés avec un `\` pour être traités comme des caractères normaux.
- Les ensembles de caractères, définis entre `[]`, permettent de rechercher des caractères spécifiques, comme `[A-Z]` pour correspondre à n'importe quelle lettre majuscule.
- Les quantificateurs, comme `+` et `*`, contrôlent le nombre de fois qu'un motif doit se répéter pour être une correspondance valide.

Pour en savoir plus sur les expressions régulières, vous pouvez consulter ces ressources supplémentaires:

## Voir aussi
- [Tutoriel Regex pour les débutants](https://regexone.com/)
- [Documentation officielle sur les expressions régulières](https://www.regular-expressions.info/)
- [Bibliothèque Regex pour Arduino](https://github.com/bblanchon/ArduinoRegexp)