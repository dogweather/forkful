---
title:                "Analisando html"
html_title:           "Fish Shell: Analisando html"
simple_title:         "Analisando html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## O que e por que?
A análise de HTML é o processo de extrair informações específicas de uma página da web, como texto, imagens ou links. Programadores muitas vezes precisam fazer isso para automatizar tarefas, como coletar dados ou testar sites. O Fish Shell pode ser uma ótima ferramenta para fazer isso.

## Como fazer:
```
Fish Shell é uma linguagem de programação poderosa, então analisar HTML é fácil com ela.
```
Para extrair o texto de um parágrafo específico em uma página da web, podemos usar o seguinte código:
```
set paragraph (curl https://exemplo.com/ | grep "<p>" | sed 's/<[^>]\+>//g')
echo $paragraph
```
Isso nos retornará o texto do primeiro parágrafo da página alvo. Se o texto que procuramos estiver dentro de tags específicas, podemos usar o comando `grep` para encontrar a tag e, em seguida, usar o comando `sed` para remover todas as tags HTML da saída.

## Mergulho profundo:
A análise de HTML tem sido uma tarefa comum para programadores desde os primeiros dias da internet. Existem várias maneiras de fazê-lo, incluindo o uso de ferramentas específicas, como o Beautiful Soup em Python, mas com Fish Shell, podemos fazer isso diretamente na linha de comando sem precisar de ferramentas adicionais.

Para a análise mais complexa de páginas da web, pode ser necessário ter conhecimento de expressões regulares e demonstrar habilidades de depuração para garantir que o código esteja funcionando corretamente.

## Veja também:
- Documentação oficial do Fish Shell: https://fishshell.com/docs/current/index.html
- Tutorial de análise de HTML com Fish Shell: https://medium.com/@Lansdon/html-parsing-with-fish-shell-c19c7c311b4a