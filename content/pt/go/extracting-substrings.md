---
title:                "Extraindo subcadeias"
html_title:           "Go: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Extrair substrings é uma tarefa comum na programação, especialmente se você está trabalhando com strings longas e complexas. É uma forma eficiente de obter exatamente a informação que você precisa sem ter que percorrer toda a string original.

## Como Fazer

Extrair substrings em Go é simples e direto. Use o operador de slice, indicando o índice de início e fim da substring que você quer extrair. Veja o exemplo abaixo:

```Go
texto := "Esta é uma string de exemplo"
substring := texto[5:12]
fmt.Println(substring) // saída: é uma s
```

Você também pode usar o método `substring()` da biblioteca `strings` para extrair substrings com base em um caractere específico. Por exemplo:

```Go
texto := "www.example.com"
substring := strings.TrimPrefix(texto, "www.")
fmt.Println(substring) // saída: example.com
```

## Profundando

Além dos métodos mencionados acima, também é possível utilizar o pacote `regexp` para extrair substrings com base em padrões de expressões regulares. Isso pode ser útil quando a substring que você deseja extrair não tem um padrão fixo, mas segue um conjunto de regras.

Por exemplo, se você quiser extrair todos os números de uma string de texto, você pode usar a função `FindAllStringSubmatch()` da `regexp` para localizar todos os padrões numéricos na string e, em seguida, extrair as substrings correspondentes. Veja um exemplo:

```Go
texto := "Meu número de telefone é (123) 456-7890."
re := regexp.MustCompile(`\d+`)
numeros := re.FindAllStringSubmatch(texto, -1)
fmt.Println(numeros) // saída: [123 456 7890]
```

Existem muitas possibilidades quando se trata de extrair substrings em Go, e é importante entender como utilizar as diferentes técnicas disponíveis para escolher a melhor abordagem para a sua situação específica.

## Veja também

- [Documentação oficial do pacote `strings`](https://golang.org/pkg/strings/)
- [Documentação oficial do pacote `regexp`](https://golang.org/pkg/regexp/)
- [Tutorial sobre expressões regulares em Go](https://youtu.be/9hK-RRbAZDQ)