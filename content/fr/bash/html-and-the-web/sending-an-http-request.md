---
date: 2024-01-20 17:58:57.903787-07:00
description: "Envoyer une requ\xEAte HTTP, c'est demander des donn\xE9es \xE0 un serveur\
  \ web. Les programmeurs font \xE7a pour interagir avec des services web, r\xE9cup\xE9\
  rer des infos,\u2026"
lastmod: '2024-02-25T18:49:54.682991-07:00'
model: gpt-4-1106-preview
summary: "Envoyer une requ\xEAte HTTP, c'est demander des donn\xE9es \xE0 un serveur\
  \ web. Les programmeurs font \xE7a pour interagir avec des services web, r\xE9cup\xE9\
  rer des infos,\u2026"
title: "Envoi d'une requ\xEAte HTTP"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Envoyer une requête HTTP, c'est demander des données à un serveur web. Les programmeurs font ça pour interagir avec des services web, récupérer des infos, ou automatiser des tâches.

## Comment faire :
```Bash
# Utilisation de cURL pour envoyer une requête GET
curl http://exemple.com/api/utilisateurs

# Réponse attendue:
# {
#   "utilisateurs": [...]
# }

# Envoi d'une requête POST avec des données
curl -d "nom=Jean&profession=developpeur" -X POST http://exemple.com/api/utilisateurs

# Réponse attendue:
# {
#   "status": "succès",
#   "id": 4
# }
```

## Exploration approfondie
Historiquement, l'envoi de requêtes HTTP était limité à des outils spécifiques comme les navigateurs web. Désormais, des outils de ligne de commande comme `curl` et `wget` ou des bibliothèques pour langages de programmation permettent de faire le même travail de façon programmatique. En plus de `curl`, il est possible d'utiliser `wget` pour des opérations simples ou des bibliothèques comme `HTTPie` pour une expérience plus conviviale. Pour créer et gérer des requêtes HTTP complexes, on peut s'appuyer sur des scripts en Bash qui utilisent ces outils, permettant ainsi d'automatiser des interactions avec les API du web.

## Voir aussi :
- Documentation de `curl` : https://curl.se/docs/manpage.html
- `HTTPie` : https://httpie.io/
- Un comparatif entre `curl` et `wget` : https://www.baeldung.com/linux/wget-vs-curl
