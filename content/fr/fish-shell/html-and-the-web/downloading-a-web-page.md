---
date: 2024-01-20 17:43:58.003232-07:00
description: "T\xE9l\xE9charger une page Web, c'est r\xE9cup\xE9rer le contenu accessible\
  \ en ligne pour l'utiliser ou le stocker localement. Les programmeurs font \xE7\
  a pour\u2026"
lastmod: 2024-02-19 22:05:16.973571
model: gpt-4-1106-preview
summary: "T\xE9l\xE9charger une page Web, c'est r\xE9cup\xE9rer le contenu accessible\
  \ en ligne pour l'utiliser ou le stocker localement. Les programmeurs font \xE7\
  a pour\u2026"
title: "T\xE9l\xE9chargement d'une page web"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Télécharger une page Web, c'est récupérer le contenu accessible en ligne pour l'utiliser ou le stocker localement. Les programmeurs font ça pour automatiser des tâches, analyser des données ou tester des sites.

## Comment faire :
Avec Fish Shell, `curl` est votre meilleur ami pour télécharger une page web. Exécutez ces commandes, c'est rapide et efficace.

```Fish Shell
# Télécharger le contenu de example.com et l'afficher sur la console
curl http://example.com

# Sauvegarder le contenu dans un fichier nommé "page.html"
curl http://example.com -o page.html
```
Sample output:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```
C'est aussi simple que ça.

## Exploration en profondeur
Avant `curl`, il y avait `ftp`, mais c'était limité aux transferts de fichiers. `curl` a su s'imposer avec la prise en charge d'une multitude de protocoles. Parmi les alternatives, il y a `wget`, qui est plutôt utilisé pour télécharger récursivement du contenu. 

En termes d'implémentation, `curl` est une librairie qu'on appelle libcurl. Elle est disponible pour diverses langues de programmation, alors que Fish Shell l'utilise directement via la ligne de commande. Pour les tâches avancées, on peut jouer avec les en-têtes HTTP, envoyer des données via POST, etc. Fish Shell rend l'exploitation de ces options assez intuitive avec sa syntaxe épurée.

## Voir également
- Documentation officielle de `curl`: https://curl.haxx.se/docs/manpage.html
- Tutoriel sur la commande `wget`: https://www.gnu.org/software/wget/manual/wget.html
- Repo GitHub de Fish Shell: https://github.com/fish-shell/fish-shell
