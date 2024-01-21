---
title:                "Envoi d'une requête HTTP"
date:                  2024-01-20T17:59:38.763211-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Envoyer une requête HTTP, c'est demander des données à un serveur web. Les développeurs font cela pour interagir avec des API, récupérer des contenus web ou communiquer entre services.

## How to: (Comment faire :)
```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	resp, err := http.Get("https://api.example.com/data")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}

	fmt.Println(string(body))
}
```
Sample output:
```plaintext
{"status":"success","data":{"id":1,"name":"Frodo Baggins"}}
```

## Deep Dive (Plongée en profondeur)
Historiquement, les requêtes HTTP étaient gérées en Go en utilisant le package `net/http`. Go a simplifié le processus au fil du temps, offrant des fonctions de haut niveau comme `http.Get`. Il y a d'autres moyens de faire des requêtes, comme le package `httputil` ou des bibliothèques tierces comme `gorilla/mux` ou `go-resty/resty`. Concernant l'implémentation, Go utilise des interfaces et des structures pour fournir une manipulation flexible des requêtes/réponses HTTP, et prend en charge la concurrence, ce qui facilite le traitement de plusieurs requêtes en parallèle.

## See Also (Voir Aussi)
- Documentation Go sur les requêtes HTTP: [http package](https://pkg.go.dev/net/http)