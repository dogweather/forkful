---
title:                "Fish Shell: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

#Pourquoi

L'expression régulière peut sembler intimidante pour les débutants en programmation, mais elle peut grandement faciliter la manipulation de chaînes de caractères. En utilisant l'expression régulière dans Fish Shell, vous pouvez rechercher, extraire et remplacer des motifs de texte spécifiques, ce qui vous permet de gagner du temps et d'automatiser certaines tâches.

#Comment faire

```Fish Shell
#Rechercher un mot spécifique dans une chaîne de caractères
set string "Bonjour le monde!"
echo $string | grep 'Bonjour'

#Remplacer un motif de texte par un autre
set string "Bonjour le monde!"
echo $string | sed 's/Bonjour/Hello/'
```

Dans cet exemple, nous définissons une variable "string" avec la valeur "Bonjour le monde!". En utilisant la commande "grep", nous pouvons rechercher le mot "Bonjour" dans notre chaîne de caractères et l'afficher en sortie. Ensuite, en utilisant la commande "sed", nous pouvons remplacer "Bonjour" par "Hello" dans notre chaîne de caractères et l'afficher en sortie.

#Plongée en profondeur

Les expressions régulières dans Fish Shell sont basées sur le moteur d'expressions régulières de Perl. Cela signifie qu'il existe une grande variété de motifs de recherche et de remplacement que vous pouvez utiliser, tels que les métacaractères, les quantificateurs et les classes de caractères.

Il est également possible de créer des expressions régulières avancées en combinant plusieurs motifs de recherche ou en utilisant des fonctions intégrées à Fish Shell telles que "string match".

#Voir aussi

- [Documentation Fish Shell sur les expressions régulières] (https://fishshell.com/docs/current/index.html#regular-expressions)
- [Tutoriel sur les expressions régulières pour débutants] (https://regexone.com/)
- [Cheat sheet pour les expressions régulières en Fish Shell] (https://cheatography.com/fish-shell/cheat-sheets/regular-expressions-in-fish-shell/)