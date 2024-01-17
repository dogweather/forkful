---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que e porque?

Baixar uma página da web envolve obter todos os arquivos necessários para visualizar essa página em um navegador. Programadores frequentemente fazem isso para automatizar o processo de obtenção de informações de uma página, ou para fazer análises e extração de dados.

## Como fazer:

```
Bash wget <URL da página> 
```
Essa é a maneira mais simples de baixar uma página da web usando Bash. Este comando irá baixar a página e todos os seus arquivos necessários para um diretório local.

```
Bash curl <URL da página> -O
```
Outra opção é usar o comando cURL com a opção "-O", que irá salvar a página e seus arquivos sob o mesmo nome que na página original.

## Profundidade:

Baixar páginas da web é uma tarefa importante em muitos projetos de programação. Existem diferentes ferramentas e técnicas que podem ser usadas para baixar páginas da web, mas usar Bash é uma opção conveniente e fácil. Além dos comandos apresentados acima, também é possível usar outras ferramentas como Wget, HTTrack ou Scrapy.

## Veja também:

Leia mais sobre Bash e seus comandos em [Bash Reference Manual](https://www.gnu.org/software/bash/manual/) e [Bash Scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial).