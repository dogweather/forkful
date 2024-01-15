---
title:                "Baixando uma página da web"
html_title:           "Fish Shell: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que baixar uma página da web?

Você pode querer baixar uma página da web por diversos motivos, como ter acesso offline ao conteúdo ou para realizar tarefas de desenvolvimento de sites. Independentemente do motivo, o Fish Shell oferece uma maneira simples e eficiente de realizar essa tarefa.

## Como fazer:

Para baixar uma página da web usando o Fish Shell, basta seguir os seguintes passos:

1. Abra o terminal e navegue até o diretório onde deseja salvar a página baixada.
2. Digite o seguinte comando:
```
fish -c "curl -o nome_do_arquivo.html url_da_página"
```
3. Substitua "nome_do_arquivo" pelo nome que deseja dar ao arquivo baixado e "url_da_página" pelo endereço da página que deseja baixar.

Pronto! A página será baixada e salva no diretório especificado.

## Aprofundando:

Ao utilizar o comando "curl" no Fish Shell, você pode adicionar diversas opções para personalizar a forma como a página é baixada. Por exemplo, você pode especificar um user-agent diferente para simular diferentes navegadores ou adicionar opções de autenticação, caso a página exija um login.

Além disso, é possível automatizar o processo de baixar várias páginas em sequência, por exemplo, utilizando um loop e um arquivo com os endereços das páginas desejadas.

## Veja também:

- [Documentação do comando "curl" no Fish Shell (em inglês)](https://fishshell.com/docs/current/cmds/curl.html)
- [Tutorial sobre a utilização do Fish Shell (em português)](https://dev.to/fernandodoming/the-fish-shell-b9e)
- [Exemplos de como utilizar o Fish Shell para automatizar tarefas (em inglês)](https://medium.com/@jordankanter/batch-downloading-github-projects-with-fish-ab966145fa4d)