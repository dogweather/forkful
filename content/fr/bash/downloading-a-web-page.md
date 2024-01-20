---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Télécharger une page web, c'est copier son code HTML sur votre ordinateur. Les programmeurs font cela pour analyser les données de la page, récupérer des informations importantes ou encore tester leurs propres codes.

## Comment faire:

Voyons comment faire cela en utilisant la commande `curl`. Dans une console Bash, entrez:

```Bash
curl https://exemple.com > maPage.html 
```
C'est tout. Cette commande sauve la page d'accueil de "exemple.com" dans le fichier "maPage.html". 

## Plongée en profondeur:

`Curl` a été créé en 1997 par Daniel Stenberg. Il existe d'autres outils similaires, y compris `wget` et `httpie`, mais `curl` est l'outil open-source le plus populaire. L'utilisation de `curl` peut varier légèrement selon le système d'exploitation et les paramètres du serveur web.

## Voir aussi:

- Le site officiel de curl: https://curl.se
- Une excellente explication de la différence entre curl et wget: https://daniel.haxx.se/docs/curl-vs-wget.html
- Httpie, une alternative moderne à curl et wget: https://httpie.io