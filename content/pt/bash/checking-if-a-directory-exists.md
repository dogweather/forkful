---
title:    "Bash: Verificando se um diretório existe"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe

Antes de começar a usar qualquer diretório em um programa Bash, é importante garantir que ele exista para evitar erros ou comportamentos inesperados. Verificar se um diretório existe também pode ser útil em scripts de automação, onde pode ser necessário criar um diretório se ele não existir ou alternar entre diferentes diretórios. Em resumo, verificar se um diretório existe é uma prática importante e pode garantir a correta execução de suas tarefas de programação.

## Como fazer

Há várias maneiras de verificar se um diretório existe em um programa Bash. A mais comum é usando o comando `test` ou `[[` seguido do operador `-d`, que verifica se o caminho fornecido é um diretório. Vamos ver um exemplo prático abaixo:

```Bash
dir_path="/caminho/do/diretorio"

# Usando o comando test
if test -d "$dir_path"; then
    echo "O diretório existe."
else
    echo "O diretório não existe."
fi

# Usando o operador [[
if [[ -d "$dir_path" ]]; then
    echo "O diretório existe."
else
    echo "O diretório não existe."
fi
```

No exemplo acima, atribuímos um diretório à variável `dir_path` e, em seguida, usamos o comando `test` e o operador `-d` para verificar se ele existe. Em seguida, usamos uma estrutura `if` para imprimir uma mensagem de acordo com o resultado da verificação.

Outra opção é usar o comando `mkdir` com o parâmetro `-p`, que criará o diretório fornecido caso ele não exista. No entanto, se o diretório já existir, o comando não terá efeito. Veja um exemplo abaixo:

```Bash
dir_path="/caminho/do/diretorio"

mkdir -p "$dir_path" # Cria o diretório caso não exista

if [[ -d "$dir_path" ]]; then
    echo "O diretório existe."
else
    echo "O diretório não existe."
fi
```

Além disso, também é possível usar o comando `ls` com o parâmetro `-d`, que lista apenas o diretório fornecido. Se o diretório existir, ele será listado e o comando irá retornar um código de saída igual a 0, indicando sucesso. Caso contrário, o diretório não será listado e o código de saída será diferente de 0. Veja um exemplo abaixo:

```Bash
dir_path="/caminho/do/diretorio"

if ls -d "$dir_path" >/dev/null 2>&1; then
    echo "O diretório existe."
else
    echo "O diretório não existe."
fi
```

## Profundidade

Ao verificar se um diretório existe em um programa Bash, é importante levar em consideração a estrutura do caminho fornecido. Se o caminho contiver espaços ou caracteres especiais, você precisará envolvê-lo entre aspas para que o comando funcione corretamente.

Além disso, também é possível verificar se um diretório está vazio ou se possui determinado nome usando outras opções do comando `test` ou `[[`. Você pode explorar essas opções em suas próprias pesquisas e ver como elas se encaixam em suas necessidades de programação.

## Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/)
- [Tutorial de Shell Scripting Bash para iniciantes](https://linuxize.com/post/bash-scripting-tutorial/)
- [Como criar e verificar a existência de diretórios em Bash](https://www.tldp.org/LDP/abs/html/fto.html)