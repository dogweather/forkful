---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:32:04.347826-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Le parsing HTML, c'est transformer du code HTML en une structure de données manipulable par le programme. Les programmeurs le font pour extraire des infos, manipuler des contenus, ou simplement pour comprendre la structure d'une page web.

## Comment faire :

En Haskell, on utilise des bibliothèques comme `tagsoup` ou `html-conduit` pour parser le HTML. Voici un petit exemple avec `tagsoup` :

```haskell
import Text.HTML.TagSoup

-- Fonction pour extraire tous les liens d'une page HTML
extraireLiens :: String -> [String]
extraireLiens html = [href | TagOpen "a" attrs <- parseTags html, ("href", href) <- attrs]

-- Utilisation sur un morceau de HTML
main :: IO ()
main = do
    let htmlSample = "<html><head></head><body><a href='https://example.com'>Example</a></body></html>"
    print $ extraireLiens htmlSample
```

Sortie :

```
["https://example.com"]
```

## Plongée profonde

Le parsing HTML est né de la nécessité de comprendre et manipuler les pages web dynamiquement. Historiquement, des langages comme Perl étaient très utilisés pour le parsing grâce à leur puissance de traitement de texte. En Haskell, le parsing est souvent effectué en utilisant des monades pour gérer les erreurs et les états de manière élégante.

À côté de `tagsoup`, qui est souple et tolère bien le HTML mal formé, il y a `html-conduit` basé sur la librairie plus stricte `xml-conduit`. Pour les applications plus robustes, `html-conduit` offrira une structure plus rigoureuse.

La particularité de l'implémentation en Haskell réside dans son typage fort et sa gestion des effets secondaires, ce qui favorise des parsers fiables et maintenables. Les fonctions comme `parseTags` transforment le HTML en liste de tags que l'on peut facilement interroger, réduire ou transformer.

## Voir aussi

- La documentation de `tagsoup`: http://hackage.haskell.org/package/tagsoup
- Le package `html-conduit` pour une approche différente: http://hackage.haskell.org/package/html-conduit
- Un tutoriel complet sur le parsing en Haskell : https://wiki.haskell.org/Parsing_a_document
- Pour aller plus loin sur les monades, un concept clé en Haskell : https://wiki.haskell.org/Monads