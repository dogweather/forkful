---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi ?
Télécharger une page web, c'est prendre une copie du contenu de cette page depuis un serveur et le stocker localement. Les programmeurs font cela pour analyser, tester ou archiver les pages.

## Comment faire :
Voici comment télécharger une page web en utilisant Fish Shell. Utilisons `curl` :

```Fish Shell
# Télécharger une page web
function telecharge_page
    curl $argv[1] -o $argv[2]
end

# Utilisation
telecharge_page https://www.example.com myfile.html
```

Le contenu de `www.example.com` sera stocké dans `myfile.html`.

## Plongée profonde :
Historiquement, la manière de télécharger une page web a évolué à mesure que la technologie internet de même. Fish Shell, sorti en 2005, a été conçu pour simplifier l'écriture de scripts et réduire les erreurs courantes.

Il existe d'autres alternatives pour télécharger une page web, notamment `wget` qui a ses propres avantages et inconvénients.

En interne, lorsque vous téléchargez une page web, vous envoyez une requête HTTP GET au serveur. Le serveur répond avec le contenu de la page que vous enregistrez ensuite sur votre machine.

## Voir aussi :
Vous pouvez approfondir vos connaissances avec ces ressources :
- Documentation Fish Shell : https://fishshell.com/docs/current/index.html
- Curl vs Wget : https://daniel.haxx.se/docs/curl-vs-wget.html
- Introduction à HTTP : https://www.w3.org/Protocols/