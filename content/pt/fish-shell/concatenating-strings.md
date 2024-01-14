---
title:    "Fish Shell: Concatenando strings"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que

Você já se deparou com a necessidade de combinar várias strings em um único valor? Talvez você precise formatar um texto para exibição ou construir uma URL dinamicamente. Independentemente do motivo, a concatenação de strings é uma tarefa comum em programação e pode ser facilmente realizada usando o Fish Shell.

## Como fazer

Para concatenar strings no Fish Shell, basta usar o operador `+` entre as strings que você deseja combinar. Por exemplo:

```Fish Shell
set string1 "Olá"
set string2 "mundo"
echo $string1 $string2 # Saída: Olá mundo
```

Você também pode usar o operador `+=` para adicionar uma string a uma variável existente. Por exemplo:

```Fish Shell
set texto "Este é um"
texto += " exemplo"
texto += " de concatenação de strings"
echo $texto # Saída: Este é um exemplo de concatenação de strings
```

Vale ressaltar que o Fish Shell também oferece suporte à interpolação de strings. Isso significa que você pode inserir valores de variáveis diretamente em uma string usando o formato `{{$variável}}`. Por exemplo:

```Fish Shell
set nome "Pedro"
set sobrenome "Silva"
set nome_completo "Meu nome é {{$nome}} {{$sobrenome}}"
echo $nome_completo # Saída: Meu nome é Pedro Silva
```

## Mergulho profundo

Ao concatenar strings, é importante estar ciente de que tipo de dados você está lidando. Por exemplo, se uma das strings for um número, ele será convertido para string antes de ser concatenado. Além disso, se você estiver lidando com caracteres especiais, como aspas, pode ser necessário usar caracteres de escape para garantir que eles sejam tratados corretamente.

Outra coisa a ser considerada é o desempenho da concatenação de strings. O Fish Shell não executa a concatenação de forma otimizada por padrão, pois cada vez que uma string é concatenada, uma nova string é criada na memória. Isso pode não ser um problema para tarefas simples, mas pode afetar o desempenho em tarefas mais complexas. Nesses casos, é recomendável utilizar o comando `string join`, que combina várias strings em uma única string sem criar intermediários.

## Veja também

- Documentação oficial do Fish Shell sobre concatenação de strings
- Tutorial de concatenação de strings no Fish Shell
- Exemplos práticos de concatenção de strings no Fish Shell