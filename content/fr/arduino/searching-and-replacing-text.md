---
title:    "Arduino: Recherche et remplacement de texte"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes un programmeur expérimenté ou si vous êtes nouveau dans le monde de l'Arduino, vous savez probablement que la recherche et le remplacement de texte peuvent être des tâches fastidieuses et répétitives. Pourquoi passer du temps à modifier manuellement chaque occurrence de texte lorsque vous pouvez utiliser une méthode plus efficace et rapide?

## Comment faire
La programmation d'Arduino offre une fonctionnalité appelée "Rechercher et remplacer" qui vous permet de rechercher un certain texte dans votre code et de le remplacer par un autre texte. Voici un exemple de code qui illustre comment utiliser cette fonctionnalité:

```Arduino
// Rechercher et remplacer un texte dans une chaîne
String texte = "Bonjour tout le monde!";
texte.replace("Bonjour", "Salut");
Serial.println(texte); // Résultat: Salut tout le monde!
```

Comme vous pouvez le voir, en utilisant la fonction "replace", nous pouvons trouver et remplacer le mot "Bonjour" par "Salut" dans notre chaîne de texte. Vous pouvez également utiliser cette méthode pour remplacer du texte dans une variable, par exemple:

```Arduino
// Rechercher et remplacer dans une variable
int nombre = 24;
String variable = "Le nombre est: " + String(nombre);
variable.replace("24", "36");
Serial.println(variable); // Résultat: Le nombre est: 36
```

La fonction "replace" peut également être utilisée pour remplacer plusieurs occurrences d'un mot dans une chaîne de texte. Il suffit d'ajouter un troisième paramètre spécifiant le nombre maximum de remplacements souhaités, par exemple:

```Arduino
// Remplacer plusieurs occurrences d'un mot
String texte = "La pluie en Espagne tombe essentiellement sur la plaine.";
texte.replace("pluie", "soleil", 2);
Serial.println(texte); // Résultat: La soleil en Espagne tombe essentiellement sur la plaine.
```

## Plongée en profondeur
En plus de remplacer du texte dans des chaînes de caractères et des variables, vous pouvez également utiliser la fonction "replace" pour remplacer du texte dans des tableaux. Vous pouvez même utiliser des expressions régulières pour rechercher et remplacer des motifs spécifiques dans votre code.

Il est important de noter que la fonction "replace" modifie la chaîne d'origine, il n'y a donc pas besoin de stocker le résultat dans une nouvelle variable. De plus, si le texte recherché n'est pas trouvé dans la chaîne, aucune modification ne sera apportée.

## Voir aussi
- [Documentation officielle Arduino](https://www.arduino.cc/reference/en/language/structure/strings/functions/replace/)
- [Tutoriel sur la recherche et le remplacement de texte en Arduino](https://create.arduino.cc/projecthub/microchef/arduino-replaceall-tutorial-c6bb1e)
- [Blog sur la manipulation de chaînes de caractères en Arduino](https://www.teachmemicro.com/arduino-string-manipulation/)