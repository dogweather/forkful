---
title:                "Envoi d'une requête http"
html_title:           "Go: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
L'envoi d'une requête HTTP est le processus par lequel un client (tel qu'un navigateur web) demande des données à un serveur web. Les programmeurs effectuent cette action pour récupérer des informations à partir de ressources en ligne et les utiliser dans leur code.

## Comment faire:
Go est un excellent langage pour envoyer des requêtes HTTP en raison de son support intégré pour les opérations web. Voici un exemple de code pour envoyer une requête GET simple:

```Go
resp, err := http.Get("https://example.com")
if err != nil {
  fmt.Println("Erreur lors de la requête:", err)
}
defer resp.Body.Close()
body, err := ioutil.ReadAll(resp.Body)
if err != nil {
  fmt.Println("Erreur lors de la lecture de la réponse:", err)
}
fmt.Println(string(body))
```

La sortie de cet exemple sera le contenu de la page d'accueil de "example.com".

## Plongée en profondeur:
L'envoi de requêtes HTTP est un élément fondamental de la communication sur le web et a été créé pour permettre aux clients de récupérer des informations de serveurs distants. Il existe plusieurs alternatives à l'utilisation de Go pour envoyer des requêtes, telles que cURL ou JavaScript dans les navigateurs web. Les détails d'implémentation de Go pour l'envoi de requêtes HTTP incluent la possibilité de personnaliser les en-têtes et les méthodes HTTP utilisées.

## Voir aussi:
- Le package "net/http" de Go: https://golang.org/pkg/net/http/
- La documentation sur les requêtes HTTP: https://developer.mozilla.org/fr/docs/Web/HTTP/Overview