---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'envoi d'une demande HTTP signifie communiquer avec un serveur web pour demander ou soumettre des informations. Les programmeurs le font pour échanger des données avec des services web et interagir avec des API.

## Comment faire:

Voici comment utiliser `curl` en Bash pour envoyer une requête GET:

```Bash
#!/bin/bash
URL="http://monsite.com"
curl $URL
```

Et voici comment envoyer une requête POST:

```Bash
#!/bin/bash
URL="http://monservice.com/api"
curl -d "param1=value1&param2=value2" -X POST $URL
```

La sortie ressemblera à quelque chose comme ceci:

```Bash
{ "status": "success", "data": { ... } }
```

## Approfondissement

Historiquement, le protocole HTTP est devenu omniprésent dans les interactions entre clients et serveurs web depuis son invention en 1991. 

En ce qui concerne les alternatives à `curl`, il y a `wget`, bien que chaque outil ait ses propres avantages. `wget` est souvent préféré pour le téléchargement de fichiers en raison de sa capacité à gérer les téléchargements récursifs et à reprendre les téléchargements interrompus.

Lors de l'envoi d'une requête HTTP via Bash, `curl` effectue un certain nombre de tâches en coulisse. Il résout l'URL spécifiée, établit une connexion TCP avec le serveur, et envoie une demande HTTP formatée selon les paramètres que vous avez spécifiés. Il attend ensuite la réponse du serveur et l'affiche sur la sortie standard.

## Voir aussi

- La documentation officielle de `curl`: https://curl.se/docs/
- Pour une compréhension avancée de HTTP : https://developer.mozilla.org/fr/docs/Web/HTTP
- Guide `wget` : https://www.gnu.org/software/wget/manual/wget.html.