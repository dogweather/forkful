---
title:                "Concatenando strings"
html_title:           "Bash: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que
Concatenar strings é uma tarefa comum em programação e pode ser útil para juntar diferentes textos e variáveis em uma única string. Isso pode ser útil em várias situações, como imprimir mensagens personalizadas, criar nomes de arquivos ou URLs dinamicamente, entre outras.

## Como Fazer
```Bash
# Criando uma string a partir de variáveis
nome="João"
sobrenome="Silva"
mensagem="Olá, $nome $sobrenome, bem-vindo!"
echo $mensagem
# Saída: Olá, João Silva, bem-vindo!

# Concatenando strings manualmente
string1="Python"
string2="Bash"
concatenada=$string1$string2
echo $concatenada
# Saída: PythonBash

# Utilizando o comando `cat` para juntar strings de arquivos
cat arquivo1.txt arquivo2.txt > concatenado.txt
```

## Mergulho Profundo
Em Bash, existem várias maneiras de concatenar strings, como mostrado nos exemplos acima. Além disso, é possível utilizar o operador de concatenação `+=` para adicionar mais strings a uma variável já existente. Também é importante notar que a ordem em que as strings são concatenadas pode afetar o resultado final. Outra dica é utilizar aspas duplas `"` ao invés de aspas simples `'` para que as variáveis dentro da string sejam expandidas.

## Veja Também
- [Documentação do Bash](https://www.gnu.org/software/bash/)
- [Guia de Referência Rápida do Bash](https://shellmagic.xyz/)