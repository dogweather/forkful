---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Go: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que & Por que?

Excluir caracteres que correspondam a um padrão é uma técnica comum usada pelos programadores para remover determinados caracteres de uma string. Isso pode ser útil para limpar dados indesejados ou formatar strings de acordo com uma determinada lógica. É uma prática importante para manter o código limpo e eficiente.

## Como fazer:

```go
pattern := regexp.MustCompile(`[aeiou]`) // cria um padrão regex para vogais
str := "Olá, mundo!"

cleanStr := pattern.ReplaceAllString(str, "") // retorna "Ol, mnd!"
fmt.Println(cleanStr)
```

Aqui, usamos o pacote `regexp` padrão do Go para criar um objeto Regex com o padrão `[aeiou]`, que corresponde a todas as vogais. Em seguida, usamos o método `ReplaceAllString` para substituir todas as ocorrências desse padrão em uma string por uma string vazia, resultando em uma string sem vogais. O mesmo pode ser feito com qualquer outro padrão e pode ser facilmente integrado ao seu código.

## Profundidade:

### Contexto histórico:

O conceito de expressões regulares (ou Regex) vem de linguagens formais e teoria da computação, surgindo na década de 1950. Foi posteriormente usado por linguagens de programação para permitir a manipulação de texto de forma eficiente. No Go, a implementação do pacote `regexp` é baseada no mecanismo do Perl e atualmente usa a sintaxe POSIX para expressões regulares.

### Alternativas:

Existem vários outros métodos para manipular strings e substituir caracteres em uma string, como o uso de funções múltiplas `Replace` no pacote `strings` ou o método `Trim` para remover caracteres específicos. No entanto, o uso de expressões regulares oferece uma maneira mais versátil e eficiente de realizar essas operações.

### Detalhes de implementação:

O pacote `regexp` no Go usa a biblioteca C `re2` para implementar expressões regulares. Ele compila os padrões em um formato determinístico de automato finito que é então usado para comparar com as strings fornecidas. Isso oferece um desempenho aprimorado em comparação com a implementação padrão de expressões regulares em outras linguagens de programação.

## Veja também:

- [Documentação oficial do pacote regexp no Go](https://golang.org/pkg/regexp/)
- [Tutorial sobre expressões regulares no Go](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go-pt)