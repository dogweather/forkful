---
date: 2024-01-20 17:59:43.281965-07:00
description: "Comment faire : \xC0 l'origine, les requ\xEAtes HTTP \xE9taient un moyen\
  \ simple d'obtenir des pages web statiques. Aujourd'hui, elles sont la colonne vert\xE9\
  brale\u2026"
lastmod: '2024-04-05T22:51:12.181514-06:00'
model: gpt-4-1106-preview
summary: "\xC0 l'origine, les requ\xEAtes HTTP \xE9taient un moyen simple d'obtenir\
  \ des pages web statiques."
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

## Comment faire :
```Fish Shell
# Utilisation de la commande `curl` pour envoyer une requête GET simple
curl http://api.exemple.fr/utilisateurs

# Affichage du code de statut de la réponse HTTP
curl -o /dev/null -s -w "%{http_code}\n" http://api.exemple.fr/utilisateurs

# Envoi d'une requête POST avec des données JSON
curl -X POST -H "Content-Type: application/json" -d '{"nom":"Dupont","prenom":"Jean"}' http://api.exemple.fr/utilisateurs
```

Sortie attendue :
```
# Pour une requête GET simple
[{"id":1,"nom":"Durand","prenom":"Alice"}, ...]

# Pour l'affichage du code de statut
200

# Pour une requête POST
{"id":2,"nom":"Dupont","prenom":"Jean"}
```

## Plongée profonde
À l'origine, les requêtes HTTP étaient un moyen simple d'obtenir des pages web statiques. Aujourd'hui, elles sont la colonne vertébrale des communications sur le web, permettant les APIs REST, les services web SOAP et plus encore. Par rapport à des outils comme `wget` ou des langages comme PHP et Python qui peuvent aussi envoyer des requêtes HTTP, `curl` est souvent privilégié pour sa simplicité et sa portabilité dans les scripts shell et la ligne de commande. En Fish Shell, `curl` fonctionne comme dans n'importe quel autre terminal, mais Fish offre une syntaxe plus lisible et des fonctionnalités comme l'auto-suggestion et la coloration syntaxique qui rendent l'écriture de scripts plus agréable.

## Voir aussi
- Documentation `curl`: https://curl.se/docs/manpage.html
- Tutoriel Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Guide des codes de statut HTTP: https://developer.mozilla.org/fr/docs/Web/HTTP/Status
