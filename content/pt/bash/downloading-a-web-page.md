---
title:                "Bash: Fazendo o download de uma página da web"
simple_title:         "Fazendo o download de uma página da web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que baixar uma página da web em Bash?

Baixar uma página da web usando Bash pode ser uma tarefa muito útil para programadores e usuários em geral. Usando Bash, é possível automatizar o processo de download e extração de informações de uma página da web, economizando tempo e esforço.

## Como fazer:

Para baixar uma página da web em Bash, precisamos usar o comando `curl` seguido do URL da página que desejamos baixar. Por exemplo:

```Bash
curl https://www.exemplo.com > pagina.html
```

Este comando irá baixar a página e salvá-la em um arquivo HTML com o nome de "pagina.html".

Também podemos especificar o formato de saída, como por exemplo o formato JSON, usando a opção `-H` junto com a URL desejada:

```Bash
curl -H "Accept: application/json" https://www.exemplo.com > pagina.json
```

Com isso, baixaremos a página no formato JSON e a salvaremos em um arquivo com a extensão ".json".

## Aprofundando:

O comando `curl` também possui outros parâmetros que podem ser úteis para baixar uma página da web. Por exemplo, podemos usar a opção `-O` para salvar o arquivo com o mesmo nome da página da web que estamos baixando, e a opção `-L` para seguir redirecionamentos de URLs. Além disso, podemos adicionar a opção `-v` para ver informações detalhadas sobre o processo de download.

Também é possível usar o comando `wget` para baixar páginas da web em Bash. Este comando possui opções semelhantes ao `curl` e pode ser útil em algumas situações específicas.

## Veja também:

- [Documentação oficial do comando `curl`](https://curl.haxx.se/docs/)
- [Tutorial sobre como baixar páginas da web usando Bash](https://www.linuxjournal.com/content/downloading-entire-web-site-wget)