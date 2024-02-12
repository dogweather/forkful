---
title:                "Analyse Syntaxique du HTML"
aliases:
- /fr/fish-shell/parsing-html.md
date:                  2024-02-03T19:12:13.587974-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyse Syntaxique du HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

L'analyse du HTML consiste à extraire des données ou des informations à partir de contenus HTML, une tâche courante lorsqu'on traite des données web. Les programmateurs font cela pour automatiser l'extraction d'informations des sites web, pour des tâches telles que le web scraping, le data mining ou les tests automatisés.

## Comment faire :

Principalement, Fish shell n'est pas conçu pour analyser directement le HTML. Toutefois, il excelle à assembler des outils Unix tels que `curl`, `grep`, `sed`, `awk`, ou à utiliser des outils spécialisés comme `pup` ou `beautifulsoup` dans un script Python. Voici des exemples qui montrent comment tirer parti de ces outils depuis Fish shell pour analyser le HTML.

### Utiliser `curl` et `grep` :
Récupérer du contenu HTML et extraire les lignes contenant des liens :

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

Sortie :
```
/page1.html
/page2.html
...
```

### Utiliser `pup` (un outil en ligne de commande pour analyser le HTML) :

Tout d'abord, assurez-vous que `pup` soit installé. Ensuite, vous pouvez l'utiliser pour extraire des éléments par leurs tags, ids, classes, etc.

```fish
curl -s https://example.com | pup 'a attr{href}'
```

La sortie, similaire à l'exemple de `grep`, listerait les attributs href des balises `<a>`.

### Avec un script Python et `beautifulsoup` :

Bien que Fish lui-même ne puisse pas analyser nativement le HTML, il s'intègre de manière transparente avec les scripts Python. Voici un exemple concis qui utilise Python avec `BeautifulSoup` pour analyser et extraire des titres à partir de HTML. Assurez-vous d'avoir `beautifulsoup4` et `requests` installés dans votre environnement Python.

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

Utilisation :

```fish
parse_html 'https://example.com'
```

Sortie :
```
Domaine Exemple
```

Chacune de ces méthodes sert des cas d'usage et des échelles de complexité différentes, allant de la simple manipulation de texte en ligne de commande à la pleine puissance d'analyse de `beautifulsoup` dans les scripts Python. Selon vos besoins et la complexité de la structure HTML, vous pouvez choisir une approche de pipeline Unix simple ou une approche de scriptage plus puissante.
