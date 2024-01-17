---
title:                "Lendo argumentos de linha de comando"
html_title:           "Bash: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

O que e por que?
Ler argumentos da linha de comando é o processo de obter informações fornecidas pelo usuário ao executar um programa no terminal. Os programadores fazem isso para permitir que seus programas sejam mais flexíveis e personalizáveis, permitindo aos usuários fornecerem parâmetros e opções para controlar o comportamento do programa.

Como fazer:
```Bash
# Exemplo de código para ler argumentos da linha de comando
# Sintaxe: ./programa.sh [-n <nome>] [-a <idade>]
# Opções disponíveis: -n (nome), -a (idade)

nome="" # cria uma variável vazia para armazenar o nome fornecido pelo usuário
idade="" # cria uma variável vazia para armazenar a idade fornecida pelo usuário

# loop para iterar todos os argumentos fornecidos
while getopts "n:a:" opcao; do
  case "${opcao}" in # verifica qual opção foi fornecida pelo usuário
    n) nome=${OPTARG};; # armazena o nome fornecido pelo usuário na variável "nome"
    a) idade=${OPTARG};; # armazena a idade fornecida pelo usuário na variável "idade"
  esac
done

# verifica se o nome foi fornecido e, se sim, imprime uma mensagem personalizada
if [ -n "$nome" ]; then
  echo "Olá $nome! Você tem $idade anos?" # imprime uma mensagem personalizada
else
  echo "Olá estranho! Você tem $idade anos?" # imprime uma mensagem padrão
fi
```

Dive Dive:
Ler argumentos da linha de comando é uma técnica amplamente utilizada em programação, especialmente em linguagens de script como o Bash. Permite que os programas sejam mais flexíveis e personalizáveis, permitindo que o usuário forneça opções e parâmetros específicos ao executá-los. Além disso, também é uma forma de tornar a interação entre o usuário e o programa mais dinâmica.

Ver também:
- Documentação oficial do Bash sobre como ler argumentos da linha de comando: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- Artigo sobre como usar argumentos da linha de comando para criar scripts Bash mais eficientes: https://linuxhint.com/bash_script_command_line_arguments/