---
title:                "Enviando um pedido http"
html_title:           "C: Enviando um pedido http"
simple_title:         "Enviando um pedido http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que

Se você está desenvolvendo um aplicativo ou site que precisa se comunicar com outros servidores ou obter dados da internet, enviar uma solicitação HTTP é essencial para seu funcionamento. Com o uso do C (a versão atual), você pode facilmente enviar solicitações HTTP e receber respostas para o seu programa.

## Como fazer

Para enviar uma solicitação HTTP em C, você precisará incluir a biblioteca "http.h" e utilizar as funções "http_get" e "http_post" dependendo do tipo de solicitação que deseja fazer. Aqui está um exemplo de como enviar uma solicitação GET e imprimir a resposta no console:

```C
#include <stdio.h>
#include <http.h>

int main() {
	char *response = http_get("https://www.example.com"); // enviar solicitação GET
	printf("Resposta da solicitação HTTP: %s", response); // imprimir resposta
	return 0;
}
```

A saída do programa será algo semelhante a:

```
Resposta da solicitação HTTP: <html>
<head>
...
</head>
<body>
...
</body>
</html>
```

Para enviar uma solicitação POST, você precisará especificar os dados que deseja enviar no corpo da solicitação. Aqui está um exemplo:

```C
#include <stdio.h>
#include <http.h>

int main() {
	// dados que serão enviados no corpo da solicitação
	char *data = "nome=exemplo&idade=25&cidade=exemplo";
	char *response = http_post("https://www.example.com", data); // enviar solicitação POST
	printf("Resposta da solicitação HTTP: %s", response); // imprimir resposta
	return 0;
}
```

## Deep Dive

Ao enviar uma solicitação HTTP em C, existem vários parâmetros que você pode especificar, como o cabeçalho da solicitação e a porta que você deseja usar. Se você deseja explorar mais a fundo o envio de solicitações HTTP em C, aqui estão alguns recursos úteis:

- Documentação oficial da biblioteca "http.h": https://www.gnu.org/software/libc/manual/html_node/HTTP.html
- Tutorial sobre como enviar solicitações HTTP em C: https://www.programiz.com/c-programming/examples/send-data-http-post
- Vídeo explicando como implementar uma solicitação HTTP em C: https://www.youtube.com/watch?v=H4m1ipT5Wn0

## Veja também

- Como trabalhar com sockets em C: https://www.freecodecamp.org/news/working-with-sockets-in-c-790fad8fcc2d/
- Como utilizar a API Rest em C: https://medium.com/@cgoxo/utilizando-api-rest-em-c-b631630f920b