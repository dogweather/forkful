---
title:                "Enviando uma requisição HTTP"
date:                  2024-01-20T17:59:33.979086-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Enviar uma requisição HTTP é basicamente pedir pra um servidor web mandar alguma coisa de volta, pode ser uma página, dado ou um serviço. Programadores fazem isso para interagir com APIs, serviços web e automatizar tarefas na internet.

## How to:
```Go
package main

import (
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	resp, err := http.Get("https://api.github.com/users/github")
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}

	log.Println(string(body))
}
```
Saída:
```
{"login":"github","id":1,"node_id":"MDQ6VXNlcjE=" ...}
```

## Deep Dive
Enviar requisições HTTP é essencial desde o início da web. O Go usa um pacote `net/http` padrão que simplifica este processo. Alternativas incluem usar bibliotecas de terceiros como `gorequest` ou `resty`, mas `net/http` é mais do que suficiente pro básico. Ele lida com métodos GET, POST, e outros, além de permitir configuração de cabeçalhos e timeouts. A implementação do Go é conhecida por ser eficiente e escalável, adequada para serviços concorrentes.

## See Also
- Documentação oficial do `net/http`: https://golang.org/pkg/net/http/
- Go by Example - HTTP Clients: https://gobyexample.com/http-clients
- Writing Web Applications (tutorial do Go): https://golang.org/doc/articles/wiki/