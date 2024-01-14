---
title:                "Bash: Conversion d'une chaîne en minuscules"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est important de savoir comment convertir une chaîne de caractères en minuscules si vous travaillez avec des données textuelles dans votre code Bash. Cela peut être utile pour la manipulation de chaînes de caractères, la comparaison de données et bien plus encore.

## Comment faire

La conversion d'une chaîne de caractères en minuscules est une tâche simple en Bash grâce à la commande `tr`. Cette commande remplace chaque occurrence de caractères dans une chaîne par un autre caractère. Dans ce cas, nous allons utiliser `tr` pour convertir tous les caractères en majuscules en caractères en minuscules.

Voici un exemple de code pour convertir une chaîne de caractères en minuscules :

```Bash
# Définir une chaîne de caractères
my_string="BONJOUR TOUT LE MONDE"

# Utiliser la commande tr pour convertir en minuscules
lowercase_string=$(echo $my_string | tr 'A-Z' 'a-z')

# Afficher le résultat
echo $lowercase_string
```

Le résultat de cet exemple sera `bonjour tout le monde`, car la chaîne de caractères initiale a été convertie en minuscules.

Il est également possible d'utiliser la commande `awk` pour convertir une chaîne en minuscules. L'avantage de cette méthode est que vous pouvez spécifier la langue de la chaîne, ce qui peut être utile si vous travaillez avec des caractères spéciaux ou des lettres accentuées. Voici un exemple de code utilisant `awk` :

```Bash
# Définir une chaîne de caractères avec des caractères spéciaux
my_string="Bonjour à Tous!"

# Utiliser la commande awk pour convertir en minuscules
lowercase_string=$(echo $my_string | awk '{ print tolower($0) }')

# Afficher le résultat
echo $lowercase_string
```

Le résultat de cet exemple sera `bonjour à tous!`, avec la lettre "à" correctement convertie en minuscule.

## Plongée en profondeur

Maintenant que vous savez comment convertir une chaîne de caractères en minuscules en utilisant `tr` et `awk`, il est important de comprendre que cette conversion peut également être réalisée en utilisant des opérations de manipulation de chaînes telles que `substring`, `index`, `length`, etc. Cependant, ces méthodes peuvent être plus compliquées à mettre en œuvre et sont plus utiles pour des tâches plus complexes impliquant des chaînes de caractères.

Il est également important de noter que la conversion en minuscules dans Bash est sensible à la langue de la machine utilisée. Par exemple, si votre système utilise une langue autre que l'anglais, la commande `tr` peut ne pas fonctionner correctement car elle ne reconnaîtra pas les lettres spéciales utilisées dans d'autres langues.

## Voir aussi

- [Documentation de la commande tr](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Documentation de la commande awk](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Tutoriel sur la manipulation de chaînes en Bash](https://linuxize.com/post/bash-string-manipulation/)