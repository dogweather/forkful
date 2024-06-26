---
date: 2024-01-20 17:43:35.916974-07:00
description: 'How to: On utilise `curl` ou `wget`. Simple, rapide .'
lastmod: '2024-03-13T22:44:57.993293-06:00'
model: gpt-4-1106-preview
summary: On utilise `curl` ou `wget`.
title: "T\xE9l\xE9chargement d'une page web"
weight: 42
---

## How to:
On utilise `curl` ou `wget`. Simple, rapide :

```Bash
curl https://www.exemple.com -o ma_page.html
```

Ça sauvegarde la page d'accueil de `exemple.com` dans `ma_page.html`.

Ou avec `wget`, c'est presque pareil :

```Bash
wget https://www.exemple.com -O ma_page.html
```

Pour voir le contenu sans le sauvegarder :

```Bash
curl https://www.exemple.com
```

Sortie attendue :

```Bash
<!DOCTYPE html>
<html>
<head>
    <title>Exemple Home Page</title>
</head>
<body>
    <p>Bienvenue sur exemple.com</p>
</body>
</html>
```

## Deep Dive
Avant `curl` et `wget`, on utilisait `ftp` ou `telnet`. Pas très pratiques. `curl` est sorti en 1997, `wget` en 1996. Pourquoi deux outils ? `curl` est pour les requêtes complexes. `wget` est pour télécharger en récursif.

`curl` gère plus de protocoles et d'options d'authentification. `wget`, lui, est top pour récupérer tout un site. `curl` offre aussi une librairie (`libcurl`) pour intégrer la fonctionnalité dans d'autres logiciels.

Pourquoi ne pas utiliser un navigateur web classique ? Automatisation. En programmation, on préfère des outils qui font bien une chose sans intervention humaine. `curl` et `wget` sont stables et puissants pour ça.

## See Also
- La page man de `curl` : [curl manual page](https://curl.se/docs/manual.html)
- Documentation de `wget` : [GNU Wget Manual](https://www.gnu.org/software/wget/manual/wget.html)
- Un guide pour `curl` : [Using curl](https://ec.haxx.se/)
- Comparaison détaillée : [curl vs wget](https://daniel.haxx.se/docs/curl-vs-wget.html)
