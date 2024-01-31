---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:30:05.820770-07:00
simple_title:         "Analyse syntaxique de HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Parser du HTML, c'est l'art de décortiquer et comprendre le contenu des pages web. On le fait pour extraire des infos, automatiser des tâches, ou alimenter des applis.

## How to (Comment faire ?)
```Bash
# Utiliser curl pour récupérer le HTML
html=$(curl -s https://exemple.com)

# Utiliser grep pour extraire des données simples
echo "$html" | grep -o '<title>[^<]*' | cut -d'>' -f2
```

Sortie échantillon:
```
Le titre de la page
```

```Bash
# Pour des tâches plus complexes, utiliser xmllint
echo "$html" | xmllint --html --xpath '//h1/text()' 2>/dev/null
```

Sortie échantillon:
```
Le titre principal de la page
```

## Deep Dive (Plongée en profondeur)
Historiquement, analyser du HTML avec des outils de ligne de commande comme awk, sed ou grep était courant, mais ces outils ne sont pas conçus pour le HTML. Leurs résultats manquent de fiabilité avec des structures HTML complexes.

Alternatives:
- **BeautifulSoup** pour Python, très populaire pour des raisons de flexibilité et de facilité d'utilisation.
- **Nokogiri** pour Ruby, puissant et bien intégré.
- **Goquery** pour Go, inspiré de jQuery pour les mordus de Go.

Détails d'implémentation: Quand on analyse du HTML, c'est essentiel de respecter la structure du document (DOM). Les alternatives comme xmllint utilisent des parseurs HTML, qui saisissent la structure du DOM, contrairement aux regex ou grep.

## See Also (Voir aussi)
- Documentations `curl`: https://curl.haxx.se/docs/manpage.html
- Tutoriel `grep`: https://www.gnu.org/software/grep/manual/grep.html
- `xmllint`: http://xmlsoft.org/xmllint.html
- BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/
- Nokogiri: https://nokogiri.org/
- Goquery: https://github.com/PuerkitoBio/goquery
