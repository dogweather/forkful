---
title:                "Bash: Lendo argumentos de linha de comando"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador iniciante ou experiente em Bash, provavelmente já ouviu falar sobre o conceito de argumentos de linha de comando. Mas por que é importante entender e saber como trabalhar com eles? Os argumentos de linha de comando permitem que você personalize a execução de um programa ou script sem precisar alterar seu código. Isso torna seu código mais flexível e prático para diferentes usos.

## Como

Vamos começar com um exemplo simples para entender como os argumentos de linha de comando funcionam. Imagine que você tem um script chamado "cumprimentar.sh" que simplesmente imprime "Olá!" na tela. Se você executar o script dessa forma: `./cumprimentar.sh`, ele sempre imprimirá "Hello!". Mas, e se você quiser que ele cumprimente alguém específico? É aí que os argumentos de linha de comando entram em cena.

Podemos modificar nosso script para receber um argumento no momento da execução. Para isso, usamos a variável especial `$1`, que representa o primeiro argumento após o nome do script. Por exemplo, se executarmos `./cumprimentar.sh Ana`, o script imprimirá "Olá, Ana!" na tela.

Vamos ver o código completo do script:

```Bash
#!/bin/bash
echo "Olá, $1!"
```

Agora, se quisermos dar um cumprimento mais informal, podemos passar dois argumentos: o primeiro especificando o nome e o segundo para adicionar uma palavra extra. Por exemplo: `./cumprimentar.sh João cara legal`. O script resultará em "E aí, João cara legal!".

Além da variável `$1`, podemos usar outras, como `$2`, `$3`, e assim por diante, para trabalhar com mais de um argumento.

## Mergulho Profundo

Você pode estar se perguntando: "E se eu quiser passar vários argumentos diferentes para um script?". É possível! Podemos usar a variável especial `$@`, que representa todos os argumentos passados. Vamos adicionar essa funcionalidade ao nosso script de cumprimento:

```Bash
#!/bin/bash
echo "Olá, $@"
```

Agora, se executarmos `./cumprimentar.sh João e Maria`, o script imprimirá "Olá, João e Maria!" na tela.

Além disso, podemos fazer verificações no nosso código para garantir que os argumentos sejam passados corretamente e tratar possíveis erros.

## Veja Também

- [Documentação Oficial do Bash](https://www.gnu.org/software/bash/)
- [Tutorial de Scripting em Bash](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Exemplos de Scripts em Bash](https://oleddisplay.simsso.de/bash-snippets-examples/)