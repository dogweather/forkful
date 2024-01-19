---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O quê & Porquê?
Enviar um pedido HTTP é essencialmente pedir informações a um servidor usando HTTP (Protocolo de Transferência de Hipertexto). Programadores fazem isso para interagir com APIs da web, pegar dados para análise, entre outras coisas.

## Como fazer:
Podemos usar a ferramenta `curl` para fazer isso no Fish Shell. Suponhamos que queiramos pegar os dados de uma API:

```Fish Shell
curl "https://exemplo-api.com/dados"
```

Aqui está um exemplo que envia um POST request:

```Fish Shell
curl -X POST -d "nome=fulano&idade=30" "https://exemplo-api.com/cria_usuario"
```

O resultado seria algo como:

```Fish Shell
{"id": 101, "nome": "fulano", "idade": 30}
```

## Mergulho Profundo
Historicamente, existem muitas maneiras de enviar um pedido HTTP. Além do Curl, temos ferramentas como o cookies.io e HTTPie. A escolha depende do seu gosto e requisitos. O Fish Shell não tem uma função HTTP interna, mas a comunidade gosta de usar as ferramentas externas mencionadas.

Cada ferramenta tem suas próprias peculiaridades e detalhes de implementação. Por exemplo, a sintaxe de envio de um JSON usando HTTPie difere um pouco do curl:

```Fish Shell
http POST example-api.com/users name=John age:=30
```

## Veja Também
Para mais informações sobre o assunto, você pode conferir os links abaixo:
- Tutorial de Fish Shell: [site oficial](https://fishshell.com/docs/current/tutorial.html)
- Documentação Curl: [curl.haxx.se](https://curl.haxx.se/docs/httpscripting.html)
- Documentação HTTPie: [HTTPie](https://httpie.io/docs)
- Aprenda HTTP: [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/HTTP)