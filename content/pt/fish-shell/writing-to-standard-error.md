---
title:                "Escrevendo para o erro padrão"
html_title:           "Fish Shell: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

O que é e por que utilizamos a escrita no erro padrão em programação? 

A escrita no erro padrão é uma técnica em programação que permite ao programador enviar mensagens de erro ou de status para o terminal. Isso é útil para depuração de código e para informar o usuário sobre o andamento do programa. 

Como fazer: 

```Fish Shell 

# Para escrever no erro padrão, utilizamos o comando "echo" seguido do que se deseja imprimir. Por exemplo: 

echo "Opa, deu erro aqui!" 

# Isso irá imprimir no terminal a mensagem "Opa, deu erro aqui!" 

# Também é possível redirecionar a saída para o erro padrão utilizando o operador "2>" : 

echo "Isso é um erro" 2> erro.txt 

# Isso irá enviar a mensagem "Isso é um erro" para o arquivo "erro.txt" em vez de imprimi-la no terminal. 

# No entanto, a escrita no erro padrão ocorre automaticamente quando há um erro no código. Por exemplo: 

ls arquivo_que_nao_existe.txt 

# Isso irá imprimir a mensagem de erro "ls: arquivo_que_nao_existe.txt: Arquivo ou diretório não encontrado" no erro padrão. 

``` 

Mergulho Profundo: 

A escrita no erro padrão é uma prática comum e muito útil em programação, principalmente em linguagens de script. É uma maneira de informar o usuário sobre o andamento do programa e de ajudar na depuração de possíveis erros. Alguns programadores também utilizam a escrita no erro padrão como uma forma de documentação, adicionando mensagens explicativas durante a execução do código. Existem outras formas de lidar com erros, como a escrita no stderr (erro padrão), mas a escrita no erro padrão é a mais simples e amplamente utilizada pelos programadores. 

Veja também: 

- Guia de Referência do Fish Shell (https://fishshell.com/docs/current/index.html) fornecendo informações detalhadas sobre a escrita no erro padrão e outros comandos úteis no Fish Shell. 
- Perguntas frequentes sobre a escrita no erro padrão (https://www.tldp.org/LDP/abs/html/io-redirection.html#STDERROUT) que podem ajudar a esclarecer dúvidas adicionais.