---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Envoyer une requête HTTP, c'est demander des données à un serveur web utilisant le protocole HTTP. Les programmeurs le font pour obtenir des informations d'une source externe, ou pour interagir avec d'autres systèmes web.

## Comment faire:

Voici un petit exemple qui illustre comment envoyer une requête HTTP GET en Go. C'est plutôt simple, vous verrez.

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	resp, err := http.Get("http://example.com/")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}
	fmt.Printf("%s", body)
}
```

Dans cet exemple, on utilise `http.Get` pour envoyer notre requête. Puis, on utilise `ioutil.ReadAll` pour lire la réponse. La sortie sera le contenu de la page d'accueil du site `http://example.com/`.

## Plongée en profondeur:

Historiquement, les requêtes HTTP ont été l'un des moyens les plus courants d'interagir avec le web. Il y a d'autres alternatives comme les WebSockets pour des communications en temps réel, ou gRPC pour des systèmes d'entreprise complexes.

Une requête HTTP en Go est effectuée par le biais du package `net/http` accompagné souvent de `io/ioutil` pour lire les données de la réponse. La gestion des erreurs est nécessaire à chaque étape.

## Pour aller plus loin:

- Les détails supplémentaires peuvent être trouvés dans la documentation officielle de Go pour le package `net/http` : https://golang.org/pkg/net/http/
- Pour une perspective historique sur HTTP, ce lien est une lecture utile : https://developer.mozilla.org/fr/docs/Web/HTTP/Overview
- Pour en savoir plus sur les alternatives, voici une introduction aux WebSockets : https://developer.mozilla.org/fr/docs/Web/API/WebSocket_API

N'oubliez pas de pratiquer pour vous familiariser avec l'envoi des requêtes HTTP en Go. Bonne programmation!