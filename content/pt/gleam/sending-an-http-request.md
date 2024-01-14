---
title:                "Gleam: Enviando uma requisição http"
simple_title:         "Enviando uma requisição http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP
Muitas vezes, ao construir um aplicativo ou site, é necessário interagir com servidores externos para obter ou enviar informações. Isso é feito através de solicitações HTTP, permitindo que seu aplicativo se comunique com outras plataformas e serviços.

## Como Fazer
Em Gleam, existem algumas bibliotecas disponíveis para ajudá-lo a enviar solicitações HTTP. Aqui está um exemplo de como enviar uma solicitação GET usando a biblioteca `my_http`:

```Gleam
import my_http

let my_response = my_http.get("https://meuservidor.com/api/posts/")
```

Este código irá enviar uma solicitação GET para https://meuservidor.com/api/posts/ e armazenar a resposta na variável `my_response`. Agora, você pode usar essa resposta em seu código para processar os dados recebidos.

## Uma Visão Mais Profunda
Além disso, você também pode especificar quais headers e parâmetros incluir em sua solicitação, usando `my_http.request` e fornecendo os detalhes necessários. Você também pode lidar com erros em suas solicitações mediante o uso de `Result` types. Para mais informações sobre estas funcionalidades, você pode consultar a documentação da biblioteca `my_http`.

## Veja Também
- Documentação da biblioteca `my_http`: https://minhabiblioteca.com/my_http
- Tutorial de solicitações HTTP em Gleam: https://meusite.com/tutoriais/http-em-gleam
- Comunidade Gleam: https://meusite.com/comunidade-gleam