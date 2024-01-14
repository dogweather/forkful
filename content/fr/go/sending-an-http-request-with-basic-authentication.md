---
title:                "Go: Envoi d'une requête http avec authentification de base"
simple_title:         "Envoi d'une requête http avec authentification de base"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des API, vous avez probablement rencontré des cas où vous avez besoin d'authentifier vos requêtes. L'une des méthodes les plus courantes est l'authentification de base par le biais d'identifiants de connexion. Dans cet article, nous allons explorer comment envoyer une requête HTTP avec une authentification de base en utilisant le langage Go. 

## Comment Faire

Pour envoyer une requête HTTP avec une authentification de base en Go, nous avons besoin de quelques étapes simples :

1. Tout d'abord, nous devons importer le package `net/http` pour pouvoir effectuer des requêtes HTTP.
2. Ensuite, nous devons créer une structure `Request` en utilisant la fonction `NewRequest` du package `net/http`.
3. Nous spécifions ensuite l'URL vers laquelle nous voulons envoyer la requête, ainsi que la méthode HTTP (GET, POST, etc.).
4. Nous utilisons ensuite la fonction `SetBasicAuth` pour ajouter nos identifiants de connexion à la requête.
5. Enfin, nous utilisons la fonction `Client` du package `net/http` pour envoyer la requête et récupérer la réponse.

Voici un exemple de code qui envoie une requête GET à une API avec une authentification de base :

```Go
import (
    "fmt"
    "log"
    "net/http"
)

func main() {
    // Créer une nouvelle requête
    req, err := http.NewRequest("GET", "https://monapi.com/users", nil)
    if err != nil {
        log.Fatal(err)
    }

    // Ajouter les identifiants de connexion
    req.SetBasicAuth("utilisateur", "motdepasse")

    // Envoyer la requête et récupérer la réponse
    client := &http.Client{}
    resp, err := client.Do(req)
    if err != nil {
        log.Fatal(err)
    }
    defer resp.Body.Close()

    // Afficher la réponse
    fmt.Println(resp.Status)
}
```

Voici un exemple de sortie possible : `200 OK`

## Plongée en Profondeur 

Maintenant que nous avons vu comment envoyer une requête HTTP avec une authentification de base en utilisant Go, voyons de plus près ce qui se passe en coulisses. 

Lorsque nous utilisons la fonction `SetBasicAuth`, Go encode automatiquement les identifiants de connexion en utilisant Base64. Cela signifie que les identifiants ne sont pas envoyés en clair, mais plutôt sous la forme d'une chaîne de caractères encodée. De plus, nous utilisons le package `Client` pour envoyer la requête, qui gère automatiquement la création et la fermeture des connexions réseau, ainsi que la gestion des redémarrages en cas d'erreur.

## Voir Aussi

Voici quelques liens utiles pour en savoir plus sur l'envoi de requêtes HTTP avec une authentification de base en Go :

- Documentation officielle sur la gestion des requêtes HTTP en Go (https://golang.org/pkg/net/http/)
- Tutoriel sur l'envoi de requêtes HTTP en utilisant le package `net/http` (https://tutorialedge.net/golang/creating-simple-web-server-with-golang/)
- Article sur l'utilisation de l'authentification de base en Go (https://medium.com/@acoshift/basic-authentication-in-go-d1d05d75159a)