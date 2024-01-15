---
title:                "Envoi d'une requête http"
html_title:           "Bash: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous voulez interagir avec un serveur à distance, envoyer une requête HTTP est une étape essentielle. Cela vous permet de récupérer des données, d'envoyer des informations ou de simplement communiquer avec une application ou un site web.

## Comment Faire

Tout d'abord, vous aurez besoin d'un terminal pour exécuter des commandes Bash. Une fois que vous avez ouvert votre terminal, suivez ces étapes pour envoyer une requête HTTP :

``` Bash
# Déclarez votre méthode HTTP (GET, POST, PUT, etc.)
method="GET"

# Définissez l'URL à laquelle vous souhaitez envoyer la requête
url="https://example.com/api/data"

# Utilisez la commande curl pour exécuter la requête
# Insérez les variables que vous avez définies pour la méthode et URL
curl -X "$method" "$url"
```

En exécutant ces commandes, vous avez envoyé une requête GET à l'URL que vous avez spécifiée. Cependant, vous pouvez également ajouter des paramètres, des en-têtes ou un corps de requête en utilisant des options supplémentaires avec la commande curl. Voici un exemple de commande avec des options ajoutées :

``` Bash
# Déclarez votre méthode HTTP, comme avant
method="POST"

# Définissez l'URL, mais cette fois avec des paramètres
url="https://example.com/api/users?username=john&email=john@example.com"

# Ajoutez des en-têtes pour spécifier le type de contenu et l'acceptation de réponse
headers="-H 'Content-Type: application/json' -H 'Accept: application/json'"

# Définissez le corps de votre requête, ici avec un format JSON
body="{'username': 'john', 'password': '12345'}"

# Utilisez la commande curl avec les options et paramètres que vous avez définis
curl -X "$method" "$url" "$headers" -d "$body"
```

En utilisant la commande curl avec des options supplémentaires, vous pouvez personnaliser votre requête HTTP selon vos besoins.

## Plongée en Profondeur

Il est important de comprendre comment fonctionne une requête HTTP pour l'envoyer correctement. Une requête HTTP est composée de plusieurs parties, notamment la méthode, l'URL, les en-têtes, le corps de requête et la réponse.

La méthode définit l'action que vous souhaitez effectuer, comme GET pour récupérer des données ou POST pour en envoyer. L'URL est l'adresse à laquelle vous envoyez la requête. Les en-têtes spécifient les informations sur la requête, comme le type de contenu ou l'authentification. Le corps de requête contient les données à envoyer, telles qu'un formulaire ou un fichier. La réponse contient les informations renvoyées par le serveur, généralement au format JSON ou HTML.

Maintenant que vous comprenez les composants d'une requête HTTP, vous pouvez les manipuler en utilisant la commande curl pour interagir avec des serveurs à distance.

## Voir Aussi

Pour en savoir plus sur la commande curl et les requêtes HTTP, vous pouvez consulter les liens suivants :

- [Documentation officielle de curl](https://curl.haxx.se/docs/)
- [Guide complet sur les requêtes HTTP](https://www.tutorialspoint.com/http/http_requests.htm)
- [Vidéo explicative sur les requêtes HTTP et curl](https://www.youtube.com/watch?v=id9jW53_uNk)