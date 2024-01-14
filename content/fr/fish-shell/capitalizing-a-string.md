---
title:    "Fish Shell: Mettre en majuscule une chaîne de caractères"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi capitaliser une chaîne de caractères en Fish Shell?

Capitaliser une chaîne de caractères est utile lorsque vous avez besoin de modifier la casse d'un texte pour une présentation ou un traitement spécifique. Par exemple, si vous souhaitez afficher un nom en majuscules ou si vous avez besoin de rechercher une chaîne de caractères en ignorant la casse, la capitalisation peut être très pratique.

## Comment faire en Fish Shell?

La méthode la plus simple pour capitaliser une chaîne de caractères en Fish Shell est d'utiliser la commande "string capitalize" comme suit:

```Fish Shell
string capitalize "Bonjour le Monde!"
```

Cela renverra la chaîne de caractères "Bonjour Le Monde!" avec la première lettre de chaque mot en majuscule.

Vous pouvez également utiliser la fonction "upper" et "lower" pour mettre en majuscule ou minuscule l'ensemble de la chaîne de caractères:

```Fish Shell
set phrase "Je suis un programmeur"
echo $phrase | upper
Je suis un programmeur

echo $phrase | lower
je suis un programmeur
```

Fish Shell dispose également d'une commande "string title" qui capitalise automatiquement la première lettre de chaque mot:

```Fish Shell
string title "le monde est petit"
Le Monde Est Petit
```

## Plongée en profondeur

Si vous souhaitez personnaliser votre capitalisation en fonction de certains critères, vous pouvez utiliser la fonction "string replace" pour remplacer des lettres spécifiques par leur équivalent en majuscule ou en minuscule. Par exemple:

```Fish Shell
string replace -r "i" "I" "J'ai un chien"
J'aI un chIen
```

Ici, toutes les occurrences de "i" dans la phrase "J'ai un chien" ont été remplacées par "I".

## Voir aussi

- La documentation officielle de Fish Shell pour plus d'informations sur les commandes et fonctions liées à la manipulation de chaînes de caractères: https://fishshell.com/docs/current/cmds/string.html
- Un tutoriel sur les bases de Fish Shell: https://devdocs.io/fish/
- Un guide complet sur l'utilisation de Fish Shell pour la programmation: https://fishshell.com/docs/current/index.html