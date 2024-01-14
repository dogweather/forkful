---
title:                "Bash: Le téléchargement d'une page web"
simple_title:         "Le téléchargement d'une page web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Il existe de nombreuses raisons pour lesquelles vous pourriez vouloir télécharger une page web. Vous pourriez vouloir conserver une copie pour consultation hors-ligne, analyser le code source ou extraire des informations spécifiques. Dans cet article, nous allons expliquer comment télécharger une page web en utilisant Bash.

## Comment faire

Le téléchargement d'une page web en utilisant Bash peut sembler compliqué, mais c'est en fait assez simple. Tout d'abord, nous allons utiliser la commande `curl` pour télécharger la page web. Voici un exemple de code :

```Bash
curl www.exemple.com > page_web.html
```

Ce code va télécharger la page web à l'adresse www.exemple.com et la sauvegarder dans un fichier nommé "page_web.html" dans le répertoire courant. Vous pouvez également spécifier un chemin absolu pour enregistrer le fichier.

Si vous voulez télécharger plusieurs pages web en même temps, vous pouvez utiliser un script Bash pour automatiser le processus. Voici un exemple de code qui va télécharger trois pages web différentes :

```Bash
#!/bin/bash

# Enregistre les pages web dans un répertoire nommé "Pages"
mkdir Pages
cd Pages

# Télécharge les pages web
curl www.exemple1.com > page1.html
curl www.exemple2.com > page2.html
curl www.exemple3.com > page3.html
```

Maintenant que les pages sont téléchargées, vous pouvez les ouvrir dans votre navigateur ou utiliser des outils comme `grep` pour extraire les informations souhaitées.

## Deep Dive

Il est important de noter que les pages web peuvent être dynamiques, c'est-à-dire que leur contenu peut changer en fonction des paramètres ou de l'utilisateur qui y accède. Dans ces cas-là, vous devrez utiliser des outils plus avancés pour télécharger la page web, comme `wget` avec des options pour simuler un navigateur ou activer le support JavaScript.

De plus, certaines pages web peuvent être protégées par un accès authentifié, comme les sites de médias sociaux ou les forums en ligne. Dans ce cas, vous devrez inclure des informations d'identification dans votre commande `curl` pour pouvoir télécharger la page.

## Voir aussi

- [Tutoriel Curl pour télécharger des pages web](https://www.codegrepper.com/code-examples/bash/how+to+download+web+pages+with+curl)
- [Guide avancé pour télécharger des pages web avec wget](https://www.computerhope.com/unix/wget.htm)
- [Documentation officielle de curl](https://curl.haxx.se/docs/manpage.html)
- [Documentation officielle de wget](https://www.gnu.org/software/wget/manual/wget.html)