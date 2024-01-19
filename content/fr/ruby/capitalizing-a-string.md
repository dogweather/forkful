---
title:                "Mettre une chaîne de caractères en majuscule"
html_title:           "Ruby: Mettre une chaîne de caractères en majuscule"
simple_title:         "Mettre une chaîne de caractères en majuscule"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?
Capitaliser une chaîne signifie rendre la première lettre de chaque mot dans cette chaîne en majuscule. Les programmeurs le font pour améliorer l'aspect esthétique des textes ou pour mettre en valeur certains mots.

## Comment faire:
Voici comment vous pouvez capitaliser une chaîne en Ruby :
```Ruby
# Exemple de chaîne
chaine = "bonjour le monde"

# Utilisez la méthode 'capitalize'
chaine_cap = chaine.capitalize

# Affichez le résultat
puts chaine_cap
```
L'exécution de ce script affichera `Bonjour le monde`.

## Plongée en profondeur:
(1) Historiquement, la méthode 'capitalize' est disponible dans Ruby dès la première version.
(2) Si vous voulez capitaliser chaque mot de la chaîne, vous pourriez utiliser la méthode 'titleize' en Rails (`chaine.titleize`). (3) En interne, la méthode 'capitalize' opère en vérifiant chaque caractère de la chaîne et en le transformant en majuscule s'il s'agit de la première lettre du mot.

## Voir aussi:
Pour plus d'informations, consultez les sources suivantes :
- Ruby String capitalize: https://www.rubyguides.com/2018/10/ruby-capitalize-method/
- Ruby pour les nuls : https://www.ruby-lang.org/fr/documentation/quickstart/