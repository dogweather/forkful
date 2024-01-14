---
title:                "Bash: Convertendo uma string para minúsculo"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Ao escrever um programa em Bash, muitas vezes é necessário manipular strings. Uma das tarefas comuns é converter uma string para letras minúsculas. Isso pode ser útil para comparar strings, validar entradas de usuários ou simplesmente para fins estéticos.

## Como Fazer

Converter uma string para letras minúsculas é uma tarefa simples em Bash. Tudo que você precisa é usar o comando `tr` com o argumento `-s`, que irá substituir as letras maiúsculas por letras minúsculas.

```Bash
STRING="Exemplo De String"
echo $STRING | tr -s '[:upper:]' '[:lower:]'
```

O comando acima irá imprimir "exemplo de string". O argumento `-s` é usado para garantir que apenas uma única instância de cada letra seja exibida, evitando a repetição de letras em sequência.

Você também pode salvar o resultado em uma nova variável, assim como em qualquer outra manipulação de strings em Bash.

```Bash
LOWER_CASE_STRING=$(echo $STRING | tr -s '[:upper:]' '[:lower:]')

echo $LOWER_CASE_STRING
```

O resultado será "exemplo de string" novamente, mas agora em uma nova variável.

## Mergulho Profundo

Para entender melhor a lógica por trás da conversão de string para letras minúsculas em Bash, é importante entender como funciona o comando `tr`. Este comando é usado para traduzir ou excluir caracteres de uma entrada padrão ou de um arquivo. O argumento `-s` usado no exemplo acima significa "squeeze" e garante que apenas uma única instância de cada letra seja exibida.

Além disso, os argumentos `[:upper:]` e `[:lower:]` são usados para especificar quais letras serão substituídas. O `[:upper:]` indica todas as letras maiúsculas e o `[:lower:]` indica todas as letras minúsculas. Portanto, o comando `tr -s '[:upper:]' '[:lower:]'` basicamente substitui todas as letras maiúsculas por letras minúsculas na entrada.

## Veja Também

Existem muitas outras formas de manipular strings em Bash, como remover caracteres especiais ou substituir caracteres específicos. Para mais informações, confira os seguintes links:

- [Documentação do comando `tr`](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
- [Tutorial sobre manipulação de strings em Bash](https://www.gnu.org/software/bash/manual/html_node/String-Manipulations.html)
- [Listas de caracteres acessíveis ao `tr`](https://www.gnu.org/software/coreutils/manual/html_node/The-character-classes-alnum_002c-alpha_002c-etc.html)