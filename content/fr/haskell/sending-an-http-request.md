---
title:                "Envoi d'une requête http"
html_title:           "Haskell: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous faites partie du monde de la programmation, vous avez probablement entendu parler de HTTP. Ce protocole est utilisé pour communiquer entre les serveurs et les navigateurs web, ce qui signifie que chaque fois que vous naviguez sur internet, vous envoyez ou recevez des requêtes HTTP. Dans cet article, nous allons voir comment envoyer une demande HTTP en utilisant Haskell et pourquoi cela pourrait être utile pour votre projet.

## Comment Faire 

Pour envoyer une requête HTTP en Haskell, nous allons utiliser le package `http-client`. Pour commencer, importez le module `Network.HTTP.Client` dans votre projet.

Dans cet exemple, nous allons envoyer une simple demande GET à travers l'URL `https://jsonplaceholder.typicode.com/posts`. Tout d'abord, vous devez construire l'objet `Request` en utilisant la fonction `parseUrlThrow`. Cette fonction prend une URL en paramètre et renvoie une erreur si l'URL n'est pas valide. Ensuite, vous pouvez utiliser la fonction `httpLbs` pour exécuter la demande et recevoir une réponse sous forme de `Response ByteString`. La fonction `getResponseBody` extrait le corps de la réponse en tant que `ByteString`.

```
import Network.HTTP.Client

main :: IO ()
main = do
    request <- parseUrlThrow "https://jsonplaceholder.typicode.com/posts"
    response <- httpLbs request
    body <- getResponseBody response
    print body
```

Si vous exécutez ce code, vous devriez voir le corps de la réponse imprimé dans la console.

```
"{\"userId\": 1,\"id\": 1,\"title\": \"sunt aut facere repellat..."
```

Bien sûr, vous pouvez également envoyer des paramètres avec votre demande en utilisant la fonction `setQueryString` sur l'objet `Request`. Cela peut être utile si vous devez passer des données à travers l'URL.

```
main :: IO ()
main = do
    request <- parseUrlThrow "https://jsonplaceholder.typicode.com/posts"
    let requestWithParams = setQueryString [("userId", Just "1")]
    response <- httpLbs requestWithParams
    body <- getResponseBody response
    print body
```

Dans l'exemple ci-dessus, nous avons ajouté un paramètre "userId" avec la valeur "1" à notre demande GET. Le corps de la réponse devrait maintenant être filtré pour inclure uniquement les publications de l'utilisateur avec l'ID 1.

```
[{"userId": 1,"id": 1,"title": "sunt aut facere repellat..."...]
```

Vous pouvez également envoyer des demandes POST, PUT ou DELETE en utilisant les fonctions `httpNoBody`, `httpNoBody_` et `httpJSON`. Vérifiez la documentation pour en savoir plus sur ces fonctions et sur la façon de manipuler les réponses.

## Plongée en Profondeur 

Pour comprendre comment nos demandes sont traitées, il est utile de comprendre la structure de base d'une demande HTTP. Une demande HTTP est composée d'une ligne de requête, des en-têtes (headers) et du corps (body) en option. La ligne de requête contient la méthode de la demande (GET, POST, etc.), l'URI et la version de HTTP. Les en-têtes contiennent des informations supplémentaires sur la demande, telles que les en-têtes Accept et Content-Type. Le corps contient les données à envoyer avec la demande, généralement utilisées pour les demandes POST et PUT.

Lorsque vous utilisez `http-client` en Haskell, l'objet `Request` représente la ligne de requête. Vous pouvez ajouter des en-têtes et un corps à votre demande en utilisant les fonctions `setRequestHeaders` et `setRequestBody`. Ces fonctions prennent respectivement une liste d'en-têtes et un `RequestBody` comme arguments.

Le `RequestBody` peut être un simple `ByteString` ou un `RequestBodyStream`, qui est essentiellement un flux continu de données. Par exemple, vous pouvez utiliser un `RequestBodyStream` pour envoyer progressivement des données à un serveur en streaming, plutôt que de tout envoyer en une seule fois.

Pour recevoir une réponse, vous utilisez l'objet `Response`. Celui-ci contient la ligne de status, les en-têtes et le corps de la réponse. Vous pouvez extraire le corps en utilisant la fonction `getResponseBody`, mais il est important de noter que cette fonction va évaluer le contenu de la réponse entière en