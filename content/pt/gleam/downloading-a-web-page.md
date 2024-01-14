---
title:                "Gleam: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que baixar uma página da web?

Há várias razões pelas quais alguém pode querer baixar uma página da web. Pode ser para salvar uma cópia de backup, para acessar o conteúdo offline ou até mesmo para extrair dados específicos da página.

Independentemente da finalidade, o Gleam é uma ótima opção para realizar essa tarefa de forma eficiente e fácil. Com a sua sintaxe clara e recursos robustos, é uma linguagem perfeita para escrever códigos de script para baixar páginas da web. Neste artigo, vamos explorar como realizar essa tarefa usando Gleam.

## Como fazer:

Para baixar uma página da web usando Gleam, basta seguir estes três passos simples:

1. Importe o módulo `http` que é responsável por fazer requisições HTTP.
2. Use a função `http.get` para enviar uma solicitação GET para o URL da página desejada.
3. Armazene o corpo da resposta em um arquivo usando a função `slurp` do módulo `file`.

Aqui está um exemplo de código que baixa uma página da web e salva o conteúdo em um arquivo chamado `pagina.html`:

```Gleam
import http
import file

http.get("https://www.example.com")
|> file.slurp("pagina.html")
```

Este é um código simples que será suficiente para a maioria dos casos de uso. No entanto, existem muitas opções adicionais que podem ser usadas para personalizar ainda mais a sua solicitação HTTP, como adicionar cabeçalhos, parâmetros ou até mesmo autenticação. Você pode explorar essas opções na documentação do módulo `http`.

## Profundidade:

Ao fazer uma solicitação HTTP, é importante entender como funciona o protocolo. Ao baixar uma página da web, o Gleam envia uma solicitação para um servidor web e o servidor responde com o conteúdo da página. Além disso, a resposta também inclui informações adicionais, como códigos de status e cabeçalhos.

Compreender esses conceitos é importante se você quiser manipular o resultado da sua solicitação HTTP de forma eficaz. Por exemplo, se a solicitação retornar um código de status 404, isso significa que a página não foi encontrada. Ou se o cabeçalho `content-type` indicar que o conteúdo é um arquivo JSON, isso pode ser útil para extrair dados específicos do conteúdo.

Além disso, é importante lidar com possíveis erros durante a solicitação, como conexões perdidas ou timeouts. O Gleam possui recursos para lidar com essas situações e você pode aprimorar suas habilidades de manipulação de erros consultando a documentação.

## Veja também:

- Documentação do módulo `http`: https://gleam.run/modules/http.html
- Documentação do módulo `file`: https://gleam.run/modules/file.html
- Documentação de códigos de status HTTP: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Status

Agora você está pronto para baixar páginas da web usando Gleam! Experimente diferentes URLs e explore as opções adicionais disponíveis para as solicitações HTTP. Divirta-se codificando!