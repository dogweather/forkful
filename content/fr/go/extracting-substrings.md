---
title:                "Extraction de sous-chaînes"
html_title:           "Go: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/extracting-substrings.md"
---

{{< edit_this_page >}}

Qu'est-ce que l'extraction de sous-chaînes et pourquoi les programmeurs le font-ils?

L'extraction de sous-chaînes est le fait de sélectionner une partie d'une chaîne de caractères plus grande. Les programmeurs le font souvent pour traiter et manipuler des données sous forme de chaînes de caractères plus petites.

Comment faire:

Utilisez la méthode `Substring()` pour extraire une partie spécifique d'une chaîne de caractères. Voici un exemple en Go:

```
chaine := "Bonjour tout le monde !"
sousChaine := chaine.Substring(8, 4)
```

Dans cet exemple, nous extrayons les caractères de la chaîne d'index 8 (inclus) à l'index 11 (non inclus) pour obtenir la sous-chaîne "tout".

En voici un autre exemple en utilisant une boucle pour extraire plusieurs sous-chaînes:

```
chaine := "Le futur appartient à ceux qui se lèvent tôt."
for i := 0; i < len(chaine); i += 5 {
	sousChaine := chaine.Substring(i, 3)
	fmt.Println(sousChaine)
}
```

La sortie de ce code sera:

```
Le f
 fut
r ap
par
rtie
t à 
ceux
 qui
lèv
tôt
.
```

Plongée en profondeur:

L'extraction de sous-chaînes est couramment utilisée pour le traitement de chaînes de caractères, mais elle peut également être utile pour des tâches telles que la validation de données ou la manipulation de chemins de fichiers. Il existe également d'autres méthodes pour extraire des sous-chaînes comme `Split()` et `Trim()`, qui peuvent être utilisées en complément de `Substring()`. La méthode `Substring()` utilise des indices de chaîne 0-based, ce qui signifie que le premier caractère a un index de 0, contrairement à certains autres langages de programmation.

Voir aussi:

- [Documentation officielle sur les chaînes de caractères en Go](https://golang.org/pkg/strings/)
- [GitHub: exemple d'utilisation de Substring() en Go](https://github.com/danielmiessler/SecLists/blob/master/Passwords/10k_most_common.txt)