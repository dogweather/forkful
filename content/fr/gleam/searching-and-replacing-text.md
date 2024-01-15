---
title:                "Rechercher et remplacer du texte"
html_title:           "Gleam: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un développeur ou un utilisateur régulier de logiciels, vous savez probablement que les erreurs de frappe et les coquilles sont inévitables. Heureusement, il existe un moyen efficace de corriger ces erreurs en utilisant la fonction de recherche et de remplacement de texte dans Gleam.

# Comment faire

Pour utiliser la fonction de recherche et de remplacement dans Gleam, tout ce que vous avez à faire est d'ouvrir votre fichier de code dans votre éditeur de texte préféré et d'utiliser le raccourci clavier "Ctrl + F" pour ouvrir la barre de recherche. Ensuite, tapez le texte que vous souhaitez trouver dans le champ de recherche et le texte de remplacement dans le champ de remplacement. Enfin, appuyez sur "Remplacer tout" pour que toutes les occurrences du texte recherché soient remplacées par le texte de remplacement. Voici un exemple de code :

```Gleam
let phrase = "J'aime les pommes !"
let nouvelle_phrase = phrase.find_replace("pommes", "bananes")
io.println(nouvelle_phrase)
```
Sortie :

```
J'aime les bananes !
```

# Plongée en profondeur

Maintenant que vous savez comment utiliser la fonction de recherche et de remplacement dans Gleam, voici quelques informations supplémentaires pour vous aider à mieux comprendre son fonctionnement. En utilisant cette fonction, vous pouvez spécifier si vous souhaitez ignorer la casse des lettres ou utiliser des expressions régulières pour une recherche plus avancée. De plus, il est important de noter que cette fonction modifie uniquement la chaîne de caractères d'origine, elle ne crée pas une nouvelle chaîne.

# Voir aussi

- [Documentation officielle de Gleam](https://gleam.run/documentation/)
- [Guide de la communauté Gleam](https://github.com/gleam-lang/gleam/wiki)
- [Exemples de code Gleam](https://github.com/gleam-examples)