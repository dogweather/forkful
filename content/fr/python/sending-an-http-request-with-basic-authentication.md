---
title:                "Envoi d'une requête http avec authentification de base"
html_title:           "Python: Envoi d'une requête http avec authentification de base"
simple_title:         "Envoi d'une requête http avec authentification de base"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est important de sécuriser les communications entre un client et un serveur lors de l'échange de données sensibles telles que des informations d'identification. Envoyer une requête HTTP avec une authentification de base permet de protéger ces données en les chiffrant avant de les envoyer via Internet.

## Comment faire

Pour envoyer une requête HTTP avec une authentification de base en utilisant Python, il vous suffit de suivre les étapes suivantes :

1. Importez la bibliothèque `requests` :
```
import requests
```
2. Définissez les paramètres d'authentification de base avec votre nom d'utilisateur et votre mot de passe :
```
auth = ("username", "password")
```
3. Ensuite, utilisez la méthode `get` de la bibliothèque `requests` en spécifiant l'URL et les paramètres d'authentification :
```
response = requests.get("https://example.com", auth=auth)
```
4. Vous pouvez également spécifier un en-tête personnalisé pour plus de sécurité en utilisant le paramètre `headers` dans votre requête :
```
headers = {"Authorization": "Basic bW9tZW50OnJlYWR3YXk="}
response = requests.get("https://example.com", headers=headers)
```
5. Enfin, vous pouvez vérifier le code de statut de la réponse pour vous assurer que la requête a été réalisée avec succès :
```
if response.status_code == requests.codes.ok:
  print("Requête effectuée avec succès !")
```

## Plongée en profondeur

L'authentification de base est une méthode couramment utilisée pour sécuriser les requêtes HTTP. Elle utilise un chiffrement de base64 pour masquer les informations d'identification, mais il est important de noter qu'elle ne fournit pas une sécurité maximale car le texte chiffré peut être facilement décodé. Pour une sécurité renforcée, il est recommandé d'utiliser des méthodes d'authentification plus avancées telles que OAuth.

## Voir aussi

Pour en savoir plus sur l'envoi de requêtes HTTP avec Python, vous pouvez consulter les liens ci-dessous :

- [Documentation de la bibliothèque requests](https://requests.readthedocs.io/en/master/)
- [Tutoriel sur l'utilisation de l'authentification de base avec Python et requests](https://overiq.com/python-101/basic-authentication-in-python-using-requests/)
- [Exemple d'implémentation de l'authentification de base avec la bibliothèque requests](https://www.geeksforgeeks.org/get-post-requests-using-python/)