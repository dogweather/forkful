---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi ?

Envoyer une requête HTTP avec une authentification de base est une méthode pour accéder à des données sécurisées sur un serveur. Les développeurs l'utilisent pour garantir la sécurité et l'intégrité des données tout en permettant une connectivité fluide entre client et serveur.

## Comment faire :

Voici comment on peut implémenter cela avec Go :

```Go
package main

import (
    "net/http"
    "net/url"
)

func main() {

    client := &http.Client{}
    data := url.Values{}

    req, err := http.NewRequest("POST", "http://website.com", strings.NewReader(data.Encode()))
    if err != nil {
        log.Fatal(err)
    }

    req.SetBasicAuth("username", "password")
    req.Header.Add("Content-Type", "xyz; charset=utf-8")
    resp, err := client.Do(req)

    if err != nil {
        log.Fatal(err)
    }

    defer resp.Body.Close()
}
```

Dans cet exemple, nous créons d'abord un nouveau client HTTP. Ensuite, nous préparons notre requête POST et ajoutons l'authentification de base avec `req.SetBasicAuth`.

## Plongée profonde :

Historiquement, l'authentification de base est une part intégrante des spécifications HTTP, permettant aux applications clientes d'envoyer un nom d'utilisateur et un mot de passe avec chaque requête. Cette méthode a résisté à l'épreuve du temps en raison de sa simplicité.

Il existe plusieurs alternatives à l'authentification basique, comme l'authentification par jeton et l'authentification OAuth. Ces méthodes offrent un niveau de sécurité plus élevé et sont plus adaptées aux applications modernes complexes.

L'implémentation détaillée réside principalement dans la méthode `SetBasicAuth`. Cette fonction ajoute simplement l'en-tête `Authorization` avec la valeur `Basic {credentials}`, où `{credentials}` est la chaîne 'username:password' encodée en base64.

## Voir aussi :

- Documentation Go sur l'authentification de base : https://golang.org/pkg/net/http/#Request.SetBasicAuth
- Un guide sur HTTP Basic Access Authentication : https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Tutoriel plus approfondi sur l'authentification avec Go : https://gowebexamples.com/http-authentication/