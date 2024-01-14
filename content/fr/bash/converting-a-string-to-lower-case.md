---
title:    "Bash: Convertir une chaîne en minuscules"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent utile de convertir une chaîne de caractères en minuscules en programmation pour pouvoir comparer ou manipuler plus facilement les données. Cela peut également aider à normaliser les entrées de l'utilisateur pour une meilleure expérience utilisateur.

## Comment faire

Pour convertir une chaîne en minuscules en Bash, il suffit d'utiliser la commande `tr` avec l'option `-s` et l'argument `[:upper:]` pour spécifier les caractères en majuscule à remplacer par les caractères en minuscule. Voici un exemple de code avec un commentaire expliquant chaque étape :

```
# Déclaration d'une variable avec une chaîne de caractères en majuscules
ma_variable="BONJOUR TOUT LE MONDE"

# Utilisation de la commande tr pour convertir la chaîne en minuscules
tr -s '[:upper:]' '[:lower:]' <<< $ma_variable
```

La sortie de ce code sera `bonjour tout le monde`.

## Plongée en profondeur

La commande `tr` utilise des cartes de correspondance pour convertir des caractères en fonction de leurs positions dans l'alphabet. Grâce à l'option `-s`, on peut spécifier de remplacer une série de caractères par une autre série. Dans le cas de la conversion en minuscules, on utilise la carte `[:upper:]` pour les caractères en majuscule et `[:lower:]` pour les caractères en minuscule.

Une alternative à la commande `tr` pour convertir une chaîne en minuscules est d'utiliser la commande `sed` avec une expression régulière pour remplacer les caractères en majuscules par les caractères en minuscule. Voici un exemple de code avec `sed` :

```
# Déclaration d'une variable avec une chaîne de caractères en majuscules
ma_variable="BONJOUR TOUT LE MONDE"

# Utilisation de la commande sed pour convertir la chaîne en minuscules
sed 's/[:upper:]/[:lower:]/g' <<< $ma_variable
```

La sortie sera également `bonjour tout le monde`.

## Voir aussi

- [La documentation officielle de Bash](https://www.gnu.org/software/bash/)
- [Une introduction à la programmation en Bash](https://www.maketecheasier.com/getting-started-bash-programming/)
- [Un tutoriel sur Tutsplus pour apprendre les bases de Bash](https://code.tutsplus.com/fr/tutorials/the-basics-of-bash-for-beginners--cms-21086)