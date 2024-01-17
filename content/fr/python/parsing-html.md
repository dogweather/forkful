---
title:                "Analyser du html"
html_title:           "Python: Analyser du html"
simple_title:         "Analyser du html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/parsing-html.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi est-ce important ?

Analyser du HTML, c'est le processus de traitement et de conversion du code HTML en une structure de données utilisable par un programme. Les programmeurs le font pour extraire des informations spécifiques des pages Web, pour les traiter ou pour les manipuler dans leur code.

# Comment faire :

Voici un exemple de code Python qui utilise le module Beautiful Soup pour extraire tous les liens d'une page Web :

```Python
# import du module Beautiful Soup
from bs4 import BeautifulSoup
# définition de la page à analyser
page = '<html><body><a href="https://www.example.com">Lien 1</a><a href="https://www.example.com">Lien 2</a></body></html>'
# création d'un objet BeautifulSoup
soup = BeautifulSoup(page, 'html.parser')
# recherche de tous les liens et affichage de leur attribut "href"
for link in soup.find_all('a'):
    print(link.get('href'))
```

Cela produirait la sortie suivante :

```
https://www.example.com
https://www.example.com
```

# Plongée en profondeur :

Parser le HTML est un processus courant dans le développement Web, car il permet de récupérer des données précieuses à partir de pages Web. Les alternatives sont généralement basées sur différents langages ou techniques, telles que l'utilisation de CSS pour extraire des données ou le web scraping. La bibliothèque intégrée de Python, "html.parser", est également disponible pour l'analyse de HTML.

# Voir aussi :

- [Documentation de Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Tutoriel sur le web scraping avec Beautiful Soup](https://realpython.com/beautiful-soup-web-scraper-python/)
- [Documentation officielle Python sur l'analyse de HTML](https://docs.python.org/fr/3/library/html.parser.html)