---
date: 2024-01-20 17:58:57.903787-07:00
description: 'Comment faire : .'
lastmod: '2024-03-13T22:44:57.991273-06:00'
model: gpt-4-1106-preview
summary: .
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

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
