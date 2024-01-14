---
title:                "Fish Shell: Lendo argumentos de linha de comando"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador que já utilizou a linha de comando em seus projetos, provavelmente já precisou ler e utilizar argumentos na linha de comando. Neste post, vamos explorar como fazer isso utilizando o Fish Shell, uma ferramenta poderosa e amigável para manipulação de linha de comando.

## Como fazer

O Fish Shell possui um recurso nativo para lidar com a leitura de argumentos na linha de comando, através da variável interna $argv. Essa variável armazena uma lista contendo todos os argumentos passados na linha de comando ao chamar um script. Vamos ver um exemplo utilizando a função sleep do Fish Shell:

```
function exemplo -a param1
    echo "O parâmetro passado foi $argv[1]"
    sleep $param1
end

exemplo 5

```

Nesse exemplo, a função exemplo recebe um parâmetro, que é armazenado na variável $param1. Ao acessar o primeiro elemento da lista $argv, podemos ter acesso ao valor passado na linha de comando, que nesse caso será "5", e utilizá-lo no comando sleep para que o script aguarde 5 segundos antes de prosseguir com a execução.

## Deep Dive

Além da variável $argv, o Fish Shell também possui outras variáveis internas que podem ser úteis na leitura de argumentos. Uma delas é a variável $argc, que armazena o número de argumentos passados na linha de comando. Com isso, podemos criar loops para percorrer todos os argumentos passados, utilizando a estrutura for do Fish Shell. Por exemplo:

```
for arg in $argv
    echo "Argumento: $arg"
end
```

Com isso, podemos ter acesso a todos os argumentos passados, independentemente do número e da ordem em que foram passados.

## Veja também

- Documentação oficial do Fish Shell sobre a leitura de argumentos: https://fishshell.com/docs/current/tutorial.html#tutorial-arguments
- Tutorial do Medium sobre a leitura de argumentos com exemplos em Python, Java e Ruby: https://medium.com/@rajsek/fish-shell-reading-command-line-arguments-8507b9397951
- Outras dicas e truques do Fish Shell: https://github.com/fish-shell/fish-shell/wiki/Frequently-asked-questions#how-do-i-pass-multiple-arguments-to-a-function