---
title:                "Envoyer une requête http"
html_title:           "Go: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Tu t'es déjà demandé comment les applications comme Facebook ou Amazon obtiennent toutes les données qu'elles affichent sur ton écran ? Eh bien, c'est grâce aux requêtes HTTP, qui permettent aux applications de communiquer avec des serveurs distants et de récupérer des informations pour les afficher à l'écran. Dans cet article, nous allons voir comment envoyer une requête HTTP en utilisant Go et pourquoi cela peut être utile pour tes projets de développement.

## Comment faire

```Go
req, err := http.NewRequest("GET", "https://example.com", nil)
if err != nil {
    log.Fatal(err)
}

resp, err := http.DefaultClient.Do(req)
if err != nil {
    log.Fatal(err)
}

fmt.Println(resp.Status)
```

Dans cet exemple, nous créons une requête HTTP GET vers le site "https://example.com". Nous utilisons la méthode `NewRequest` de la bibliothèque `http` pour créer un nouvel objet de type `Request` qui représente la requête que nous souhaitons envoyer. Nous le passons ensuite à la méthode `Do` de `http.DefaultClient` pour l'exécuter. Enfin, nous récupérons la réponse sous forme d'objet `Response` et affichons son statut.

## Plongée en profondeur

Les requêtes HTTP sont un moyen simple et efficace de communiquer avec des serveurs. Elles se composent d'une ligne de requête contenant le type de méthode (GET, POST, etc.), l'URL et la version du protocole, suivie par des en-têtes de requête qui indiquent au serveur des informations supplémentaires sur la demande, et éventuellement un corps de requête contenant des données. Lorsque le serveur reçoit une requête, il renvoie une réponse contenant un statut, des en-têtes de réponse et éventuellement un corps de réponse avec les données demandées.

## Voir aussi

- [Documentation de la bibliothèque `http` en Go](https://golang.org/pkg/http/)
- [Article "Introduction à HTTP" sur MDN](https://developer.mozilla.org/fr/docs/Web/HTTP/Overview)