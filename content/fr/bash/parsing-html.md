---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi?

Analyser du HTML, c'est extraire des données à partir d'un document HTML. Les programmeurs le font pour récupérer des informations d'une page web dans un format manipulable.

## Comment faire:

Utilisons `html-xml-utils`, un utilitaire propre pour le parsing HTML en Bash. Installez-le d'abord:

```Bash
sudo apt-get install html-xml-utils
```

Ensuite, scrappez une page web avec sa structure intacte:

```Bash
curl https://example.com | hxnormalize -x
```

Maintenant, imaginez que vous voulez tous les liens `href` sur la page. Faites cela:

```Bash
curl https://example.com | hxnormalize -x | hxselect -s '\n' 'a[href]' | cut -d'"' -f2
```

Voilà, c'est du scraping HTML en Bash.

## Plongée profonde

Les premiers parsers HTML ont été développés à la naissance du web, dans les années 90. Alternativement, vous pouvez utiliser des langages plus modernes comme Python ou Javascript qui ont des bibliothèques de parsing HTML robustes, telles que BeautifulSoup et Cheerio. Cependant, Bash reste une solution rapide et portable sans l'overhead d'un langage de script complet.

L'analyseur `html-xml-utils` fonctionne en construisant un arbre de documents à partir d'une page HTML, et en appliquant ensuite des filtres de sélection sur cet arbre pour extraire les informations souhaitées.

## Voir aussi:

- Documentation officielle de `html-xml-utils`: http://www.w3.org/Tools/HTML-XML-utils/README
- Tutoriel sur le Web Scraping avec Python et BeautifulSoup: https://www.dataquest.io/blog/web-scraping-tutorial-python/
- Guide de débutant pour le Web Scraping en JavaScript avec Node.js: https://www.freecodecamp.org/news/the-ultimate-guide-to-web-scraping-with-node-js-daa2027dcd3/