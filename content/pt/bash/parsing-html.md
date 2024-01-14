---
title:                "Bash: Analisando html"
simple_title:         "Analisando html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/parsing-html.md"
---

{{< edit_this_page >}}

# Por que fazer uma programação Bash casual?

Se você está familiarizado com Linux, provavelmente já ouviu falar do Bash - um dos shell mais populares no sistema operacional. Mas você sabia que ele também pode ser usado para escrever scripts e automatizar tarefas? Uma dessas tarefas é a análise de HTML, o que pode ser útil para diversos fins, como extrair dados de um site ou criar um programa de coleta de informações.

## Como fazer a análise de HTML usando Bash

A primeira coisa que você precisa é de um arquivo HTML para analisar. Você pode obtê-lo através do comando `curl` ou usar um arquivo local. Em seguida, é necessário usar um comando de processamento de texto como `grep` para encontrar o conteúdo que deseja extrair. Vamos dar uma olhada em um exemplo de como fazer isso:

```Bash
html="<p>Olá, mundo!</p>"
echo "${html}" | grep -o '<p>.*</p>'
```

Neste exemplo, definimos uma variável `html` com uma tag `<p>` contendo uma frase. Ao usarmos `grep` com o argumento `-o`, ele irá imprimir apenas o trecho que corresponde ao padrão especificado - no caso, tudo que está entre as tags `<p>` e `</p>`. O resultado seria `"<p>Olá, mundo!</p>"` - a tag e seu conteúdo. 

Mas e se quisermos apenas a frase sem as tags? Podemos alterar um pouco nosso comando:

```Bash
html="<p>Olá, mundo!</p>"
echo "${html}" | grep -o '<p>\(.*\)</p>' | sed 's/<[^>]\+>//g'
```

Adicionamos o comando `sed` para substituir todas as tags pela string vazia, resultando apenas na frase desejada. O comando `\(.*\)` captura o texto entre as tags para que possa ser usado posteriormente no comando `sed`.

## Aprofundando na análise de HTML com Bash

A análise de HTML usando Bash pode ir além dessa simples extração de dados. Existem diversos comandos e ferramentas que podem ser usados, como `awk` e `curl`. Além disso, é possível combinar vários comandos para realizar análises mais complexas, por exemplo, coletando dados de diversos sites ao mesmo tempo. Há também a opção de criar scripts para automatizar essas tarefas e torná-las ainda mais eficientes.

A análise de HTML com Bash pode não ter a mesma facilidade e recursos de uma linguagem de programação voltada especificamente para web, mas ainda sim pode ser uma alternativa viável para tarefas simples de extração de dados. Além disso, o Bash é uma ferramenta extremamente útil para programadores e administradores de sistemas, então conhecer suas funcionalidades pode ser valioso em diversas situações.

## Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/)
- [Grep tutorial](https://www.digitalocean.com/community/tutorials/how-to-use-grep-commands-to-match-strings-and-files-in-linux)
- [Como usar o Bash para extrair dados de um arquivo HTML](https://www.linuxjournal.com/content/processing-html-bash-script)
- [10 exemplos úteis de scripts Bash para iniciantes](https://www.tecmint.com/bash-conditional-statements/)