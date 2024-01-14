---
title:                "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que
 O envio de solicitações HTTP é uma parte crucial da programação Bash, permitindo que você se comunique com servidores e obtenha informações valiosas. Saber como enviar uma solicitação HTTP eficiente é essencial para qualquer desenvolvedor que trabalhe com Bash.

## Como fazer
Para enviar uma solicitação HTTP utilizando Bash, você precisará utilizar o comando `curl`. Este comando permite que você envie solicitações HTTP e receba respostas do servidor. Veja um exemplo básico de como utilizar o comando `curl` para enviar uma solicitação HTTP GET:

```Bash
curl https://www.example.com
```
Isso enviará uma solicitação GET para o site "example.com" e retornará a resposta do servidor. Você também pode adicionar opções ao comando `curl` para personalizar sua solicitação, como adicionar cabeçalhos e autenticação. Por exemplo:

```Bash
curl -H "Content-Type: application/json" -d '{"username":"exemplo", "password":"senha"}' -X POST https://www.example.com/login
```
Neste exemplo, estamos enviando uma solicitação POST com um corpo JSON e definindo um cabeçalho de "Content-Type". A opção `-X` permite especificar o método HTTP desejado.

Você também pode utilizar variáveis e loops para automatizar o envio de várias solicitações HTTP. Por exemplo:

```Bash
for ((i = 0; i < 10; i++)); do
    curl https://www.example.com/usuarios/$i
done
```
Isso criará uma solicitação GET para o endpoint "/usuarios" com um número diferente de usuário a cada iteração do loop.

## Mergulho profundo
Para entender melhor como enviar solicitações HTTP em Bash, é importante conhecer os diferentes componentes de uma solicitação. Uma solicitação HTTP consiste em um método (GET, POST, PUT, DELETE), um cabeçalho e um corpo de requisição (opcional). O método é especificado pelo parâmetro `-X` no comando `curl`, enquanto o cabeçalho e o corpo são adicionados com as opções `-H` e `-d`, respectivamente.

Além disso, é importante estar ciente dos diferentes códigos de status que podem ser retornados pelo servidor em resposta à sua solicitação. Isso pode ajudá-lo a identificar problemas ou erros em sua solicitação. Alguns códigos de status comuns incluem:

- 200: resposta OK, solicitação bem-sucedida
- 400: erro de solicitação, algo está errado com a sua solicitação
- 401: erro de autenticação, suas credenciais são inválidas
- 404: página não encontrada, o endpoint que você está tentando acessar não existe

## Veja também
- [Documentação oficial do comando curl](https://curl.haxx.se/docs/manual.html)
- [Lista de códigos de status HTTP](https://pt.wikipedia.org/wiki/Lista_de_c%C3%B3digos_de_estado_HTTP)