---
title:                "Removendo caracteres que correspondem a um padrão"
html_title:           "Go: Removendo caracteres que correspondem a um padrão"
simple_title:         "Removendo caracteres que correspondem a um padrão"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Por que

Já se deparou com a necessidade de deletar caracteres de uma string que seguem um padrão específico? Isso pode ser útil em várias situações, como remoção de espaços em branco ou caracteres especiais. Neste artigo, vamos explorar como fazer isso em Go de forma simples e eficiente.

# Como fazer

A função `strings.ReplaceAll()` é a nossa aliada nessa tarefa. Ela recebe três argumentos: a string original, o padrão a ser encontrado e o padrão a ser substituído. Veja um exemplo:

```Go
nome := "João, Maria, Pedro"
nome = strings.ReplaceAll(nome, ", ", " e ")
fmt.Println(nome)
```

O resultado será `João e Maria e Pedro`, onde os caracteres ", " foram substituídos por " e ". É importante destacar que essa função substitui todas as ocorrências do padrão, não apenas a primeira.

Caso você queira remover um determinado padrão da string, basta passar uma string vazia `""` como segundo argumento. Por exemplo, se quisermos remover todos os hífens de um CEP, podemos usar:

```Go
cep := "12345-678"
cep = strings.ReplaceAll(cep, "-", "")
fmt.Println(cep)
```

O resultado será `12345678`, onde todos os hífens foram removidos da string original.

# Deep Dive

Agora, vamos mergulhar um pouco mais fundo e entender como a função `strings.ReplaceAll()` funciona. Primeiramente, ela utiliza o pacote `strings` da biblioteca padrão do Go. Esse pacote possui várias funções úteis para o tratamento de strings.

Ao receber as três strings como argumento, a função `strings.ReplaceAll()` converte todas elas em slices de bytes, o que permite uma manipulação mais eficiente. Em seguida, ela procura pelo padrão dentro da string original e, quando encontra, substitui pelo padrão desejado.

Uma alternativa para substituir os caracteres dentro de uma string é utilizar a função `strings.Map()`. Ela recebe uma função como argumento que será aplicada a cada caractere da string. No entanto, a função `strings.ReplaceAll()` tende a ser mais rápida e eficiente, principalmente em casos onde o padrão é conhecido.

# Veja também

- [Documentação oficial da função strings.ReplaceAll](https://golang.org/pkg/strings/#ReplaceAll)
- [Artigo sobre o pacote strings](https://golang.org/pkg/strings/)