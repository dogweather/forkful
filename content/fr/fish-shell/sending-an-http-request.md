---
title:                "Envoyer une requête http"
html_title:           "Fish Shell: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Une requête HTTP est une action que les développeurs utilisent pour demander et récupérer des données d'un serveur web. Ces requêtes sont cruciales pour connecter vos applications/web à d'autres systèmes et obtenir ou envoyer des données.

## Comment faire :

Maintenant, voyons comment faire une requête HTTP avec Fish Shell. Vous aurez besoin de l'outil `curl` pour cela.

```fish
# Pour obtenir une ressource
curl -X GET "http://monsite.com/api/resource"

# Pour envoyer (POST) des données 
curl -X POST -d "param1=valeur1&param2=valeur2" "http://monsite.com/api/resource"
```

Dans l'exemple ci-dessus, `-X` spécifie la méthode HTTP à utiliser et `-d` spécifie les données à envoyer. Voici à quoi pourrait ressembler la sortie :

```fish
# Exemple de sortie pour un GET
<!DOCTYPE html>...
```

## Plongeons plus profondément

Les requêtes HTTP sont au cœur d'Internet depuis les années 90. Avant `curl`, des outils comme `telnet` étaient utilisés pour faire des requêtes HTTP, mais ils étaient plus compliqués à utiliser.

Il existe des alternatives à `curl`, comme `wget` ou des bibliothèques dans divers langages de programmation pour envoyer des requêtes HTTP. Cependant, `curl` reste un outil populaire et largement supporté.

En termes de détails d'implémentation, Fish Shell ne fait que passer la commande à `curl`. Ce dernier fait tout le travail en coulisses pour envoyer la requête et récupérer la réponse.

## Voir aussi:

- [Documentation officielle de curl](https://curl.haxx.se/docs/manpage.html)
- [Documentation de Fish Shell](https://fishshell.com/docs/current/index.html)