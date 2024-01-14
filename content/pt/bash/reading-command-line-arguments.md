---
title:                "Bash: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade essencial para qualquer programador que trabalhe com Bash. Ao utilizar argumentos da linha de comando, você pode tornar seu código mais dinâmico e interativo, fornecendo ao usuário opções e informações adicionais para personalizar a execução do seu script. Além disso, a capacidade de ler argumentos da linha de comando é uma habilidade desejável para trabalhar em ambientes de desenvolvimento profissionais, onde é comum a utilização de ferramentas de linha de comando.

## Como fazer:

Para ler argumentos da linha de comando em Bash, primeiro você precisa compreender a estrutura básica do comando. Estes argumentos são passados ao script como parâmetros, que são armazenados em variáveis especiais chamadas de "positional parameters".

Vamos dar uma olhada em um exemplo simples:

```Bash
#!/bin/bash

# Este script recebe dois argumentos da linha de comando e exibe o conteúdo

echo "O primeiro argumento é $1"
echo "O segundo argumento é $2"
```

Neste exemplo, o primeiro argumento passado após o nome do script seria exibido na tela com a mensagem "O primeiro argumento é" antes. O segundo argumento seria exibido com a mensagem "O segundo argumento é" antes. Simples, certo?

Existem outras variações para a leitura de argumentos, como a utilização de opções e flags usando o comando `getopts`, mas este é um bom ponto de partida para entender como ler argumentos da linha de comando em Bash.

## Deep Dive:

É importante ressaltar que a ordem dos argumentos é importante. Se você chamar um argumento de forma errada, pode obter resultados inesperados ou até mesmo erros em seu código. Além disso, é possível acessar argumentos posicionais em qualquer ordem, desde que você expresse corretamente qual argumento você está querendo acessar.

É possível passar quantos argumentos quiser para um script em Bash, basta utilizar as variáveis especiais apropriadas. Também é possível checar quantos argumentos foram passados para um script utilizando o comando `($#)`.

## Veja também:

- [Documentação oficial do Bash sobre leitura de argumentos da linha de comando](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion)
- [Artigo sobre a importância da leitura de argumentos da linha de comando em scripts](https://www.howtogeek.com/67469/the-best-use-of-command-line-arguments-in-bash/)
- [Tutorial em vídeo sobre a leitura de argumentos da linha de comando em Bash](https://www.youtube.com/watch?v=vL49X3NZ_as)