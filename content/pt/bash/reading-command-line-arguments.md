---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Ler argumentos de linha de comando é a obtenção de input de usuário através do terminal no início de um script. Programadores fazem isso para personalizar a execução de seus scripts de maneira flexível e eficiente.

## Como fazer:
Aqui está um simples exemplo da leitura de argumentos de linha de comando em Bash:

```Bash 
#!/bin/bash
echo "Nome do script: $0"
echo "Primeiro argumento: $1"
echo "Segundo argumento: $2"
echo "Todos argumentos: $@"
```
Então, se invocássemos esse script com `./script.sh argumento1 argumento2`, a saída seria:

```Bash
Nome do script: ./script.sh
Primeiro argumento: argumento1
Segundo argumento: argumento2
Todos argumentos: argumento1 argumento2
```

## Um Mergulho Profundo
Historicamente, esta funcionalidade é fundamental para a eficiência do terminal, em programas Unix desde os primórdios. Infelizmente, há limites quanto ao número de argumentos ou tamanho dos argumentos que podem ser repassados, devido às restrições de memória no kernel Unix nos primórdios.

Alternativamente, a entrada do usuário pode ser lida durante a execução do script, mas isso não é tão flexível ou eficiente quanto o uso de argumentos de linha de comando. Um exemplo seria usando a função 'read' em Bash.

Os argumentos de linha de comando são lidos para as variáveis especiais $0, $1, $2 e assim por diante, onde $0 é geralmente o nome do script e $1, $2, etc., são os argumentos em ordem. O $@ é uma variável especial que contém todos os argumentos.

## Veja Também
Para mais detalhes e exemplos, veja estas fontes:

1. [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_03_02.html)
2. [How to Read Command Line Arguments in a Bash Script](https://www.tutorialspoint.com/how-to-read-command-line-arguments-in-a-bash-script)
3. [Command Line Arguments in Shell Scripting](https://www.geeksforgeeks.org/command-line-arguments-in-shell-scripting/)