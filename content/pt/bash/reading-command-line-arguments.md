---
title:                "Lendo argumentos da linha de comando"
html_title:           "Bash: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando com Bash?

Você deve estar se perguntando por que alguém se interessaria em ler argumentos da linha de comando com Bash, não é mesmo? Bem, a resposta é simples: isso pode facilitar muito a vida do desenvolvedor!

Quando você executa um programa ou script Bash, é possível passar informações adicionais através de argumentos na linha de comando. Isso pode ser especialmente útil para automatizar tarefas ou para tornar seu código mais flexível, permitindo que ele receba diferentes entradas.

## Como fazer isso na prática?

Para ler argumentos da linha de comando em Bash, utilizamos a variável "$@" que representa todos os argumentos passados. Por exemplo:

```Bash
#!/bin/bash

for arg in "$@" 
do 
    echo $arg 
done
```

Ao executar o script acima passando alguns argumentos, teremos como saída cada um dos argumentos em uma linha separada. Por exemplo, se executarmos "bash meu_script.sh arg1 arg2 arg3", obteremos:

```
arg1
arg2
arg3
```

Além disso, também podemos acessar diretamente cada argumento através das variáveis "$1", "$2", "$3" e assim por diante, conforme a posição do argumento na linha de comando. Por exemplo:

```Bash
#!/bin/bash

echo "O primeiro argumento é $1"
echo "O segundo argumento é $2"
```

Ao executar o script acima com os argumentos "primeiro" e "segundo", teremos como saída:

```
O primeiro argumento é primeiro
O segundo argumento é segundo
```

## Aprofundando um pouco mais

Existem muitas possibilidades quando se trata de ler argumentos da linha de comando em Bash. Além das opções apresentadas acima, é possível também utilizar a estrutura "case" para tratar diferentes ações de acordo com os argumentos recebidos, ou ainda verificar se determinado argumento foi ou não passado utilizando as estruturas condicionais "if" e "else".

Uma dica importante é sempre utilizar aspas ao redor das variáveis que representam os argumentos (como "$@") para evitar que o Bash as separe em diferentes palavras caso o argumento contenha espaços em branco.

Não se esqueça também de utilizar o comando "getopts" para facilitar a leitura de argumentos em formato de opções, como "-a" ou "--help".

## Veja também

- [Guia de argumentos da linha de comando em Bash da Wiki do Linux](https://en.wikipedia.org/wiki/Command-line_interface#Command-line_arguments)
- [Documentação oficial do Bash](https://www.gnu.org/software/bash/)
- [Introdução a Bash scripting do DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-write-a-simple-bash-script-on-ubuntu-18-04-pt)