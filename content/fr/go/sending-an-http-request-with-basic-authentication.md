---
title:                "Envoi d'une demande http avec authentification de base"
html_title:           "Go: Envoi d'une demande http avec authentification de base"
simple_title:         "Envoi d'une demande http avec authentification de base"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

L'envoi de requêtes HTTP avec une authentification de base est une pratique courante dans la programmation. Cela permet d'assurer la sécurité des données échangées entre le client et le serveur. Par exemple, lors de la connexion à un site web protégé par un mot de passe, votre navigateur envoie une requête HTTP avec l'authentification de base pour que le serveur puisse vérifier si vos informations de connexion sont valides.

## Comment faire

Pour envoyer une requête HTTP avec l'authentification de base en utilisant le langage de programmation Go, vous devez suivre les étapes suivantes :

1. Importez le package "net/http" pour pouvoir utiliser les fonctionnalités liées aux requêtes HTTP.
2. Définissez les informations d'authentification, c'est-à-dire le nom d'utilisateur et le mot de passe à envoyer avec la requête. Vous pouvez les stocker dans une variable ou les récupérer depuis une source externe.
3. Utilisez la fonction `http.NewRequest` pour créer une nouvelle requête avec la méthode HTTP, l'URL et les en-têtes requis.
4. Appelez la méthode `SetBasicAuth` sur la requête créée pour lui fournir les informations d'authentification.
5. Enfin, envoyez la requête en utilisant la fonction `http.DefaultClient.Do` et récupérez la réponse pour la traiter.

Voici un exemple de code qui envoie une requête GET à l'API GitHub en utilisant l'authentification de base :

```
package main

import (
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {

    // Définir les informations d'authentification
    username := "votre_nom_dutilisateur"
    password := "votre_mot_de_passe"

    // Créer une nouvelle requête avec l'URL cible
    req, err := http.NewRequest("GET", "https://api.github.com/user/repos", nil)
    if err != nil {
        panic(err)
    }

    // Ajouter l'authentification de base à la requête
    req.SetBasicAuth(username, password)

    // Envoyer la requête et récupérer la réponse
    resp, err := http.DefaultClient.Do(req)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // Lire la réponse en tant que chaîne de caractères
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }

    // Afficher la réponse
    fmt.Println(string(body))
}
```

Lorsque vous exécutez ce code, vous devriez voir la liste de vos dépôts sur GitHub s'afficher dans la console.

## Plongée en profondeur

L'authentification de base est une méthode d'authentification très simple mais peu sécurisée. Elle envoie les informations d'identification en clair dans la requête HTTP, ce qui les rend vulnérables aux attaques par interception. Il est recommandé d'utiliser d'autres méthodes d'authentification plus sécurisées, comme OAuth ou l'authentification à clé publique/privée.

En outre, il est important de noter que certains fournisseurs de services peuvent exiger une authentification plus complexe. Dans ce cas, vous devrez peut-être utiliser une librairie externe pour gérer l'authentification avec ce fournisseur spécifique.

## Voir aussi

- https://golang.org/pkg/net/http/ - Documentation officielle du package "http" en Go.
- https://developer.github.com/v3/#authentication - Informations sur l'authentification pour l'API GitHub.
- https://oauth.net/2/ - Informations sur le protocole OAuth.