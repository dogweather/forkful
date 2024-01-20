---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La conversion d'une chaîne en minuscules signifie changer toutes les lettres majuscules de la chaîne en minuscules. Les programmeurs le font pour faciliter les comparaisons de chaînes, en évitant des différences due à la casse.

## Comment faire:
Voici comment le faire en Gleam:

```Gleam
import gleam/string

string.to_lower("SALUT MONDE")
```

En sortie, vous obtiendrez "salut monde".

## Approfondissement
Historiquement, la conversion de caractères de majuscules à minuscules est un concept qui a pris naissance avec l'évolution des langues écrites. En programmation, c'est une fonctionnalité présente depuis l'invention des premiers langages.

Il existe toujours des alternatives à la fonction `string.to_lower()`, par exemple en itérant à travers chaque lettre et en transformant manually chaque majuscule en minuscule. Mais cette technique est beaucoup plus lente et moins efficace que d'utiliser une fonction intrinsèque comme celle fournie par Gleam.

En interne, la méthode `string.to_lower()` en Gleam parcourt chaque caractère de la chaîne et utilise la table de correspondance Unicode pour trouver l'équivalent en minuscule. 

## Voir également
Pour des informations plus détaillées, consultez les liens suivants:
1. Documentation officielle de Gleam: https://gleam.run/documentation/
2. Guide sur Unicode et les chaînes de caractères: https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/
3. Répertoire Github de Gleam : https://github.com/gleam-lang/gleam