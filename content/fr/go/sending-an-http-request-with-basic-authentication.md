---
title:                "Envoi d'une requête HTTP avec authentification de base"
date:                  2024-01-20T18:01:34.036200-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP avec authentification de base"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Envoyer une requête HTTP avec une authentification de base signifie joindre vos identifiants (nom d'utilisateur et mot de passe) pour accéder à une ressource protégée. Les programmeurs font cela pour interagir avec des APIs sécurisées ou des services web qui exigent une identification.

## Comment faire :
```Go
package main

import (
	"fmt"
	"net/http"
	"encoding/base64"
)

func main() {
	client := &http.Client{}
	req, err := http.NewRequest("GET", "https://exemple.com/ressource", nil)
	if err != nil {
		panic(err)
	}

	req.Header.Add("Authorization", "Basic " + base64.StdEncoding.EncodeToString([]byte("user:password")))
	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	fmt.Println("Status Code:", resp.StatusCode)
}
```
Sortie (exemple) :
```
Status Code: 200
```

## Plongée profonde
Historiquement, l'authentification de base HTTP a été conçue pour permettre une méthode simple d'identification. Bien qu'elle soit moins sécurisée que d'autres méthodes comme l'authentification Digest ou OAuth, elle reste largement utilisée pour sa simplicité. En pratique, les données d'identification sont encodées avec Base64, qui n'est pas un chiffrement mais juste un encodage. Les alternatives contemporaines incluent les tokens JWT ou l'utilisation d'API keys. Ces méthodes sont à privilégier pour une meilleure sécurité.

En Go, l'exécution d'une telle authentification nécessite l'utilisation du package `net/http` et souvent `encoding/base64`. Il est crucial d'utiliser HTTPS pour protéger les identifiants lors de l'envoi. Bien que l'exemple ci-dessus utilise la méthode GET, la même logique d'authentification s'applique à d'autres méthodes HTTP comme POST, PUT, etc.

## Voir également
- Documentation Go pour `net/http`: [https://pkg.go.dev/net/http](https://pkg.go.dev/net/http)
- Spécifications HTTP Basic Authentication: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- JWT (JSON Web Tokens): [https://jwt.io/](https://jwt.io/)
