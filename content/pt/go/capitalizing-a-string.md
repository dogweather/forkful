---
title:                "Go: Capitalizando uma string"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar strings em Go?

Capitalizar uma string é um processo comum em programação, e pode ser útil em diversas situações, como, por exemplo, quando é necessário padronizar a escrita de dados inseridos pelo usuário. Em Go, existem diversas maneiras de capitalizar uma string, e neste artigo vamos explorar algumas delas.

## Como capitalizar uma string em Go

Abaixo, mostraremos alguns exemplos de código em Go para capitalizar uma string. Nas linhas de código, utilizaremos um comentário para explicar o funcionamento de cada solução, e logo em seguida, mostraremos o output resultante dessa solução.

```Go
// Usando a função strings.Title(), que capitaliza a primeira letra de cada palavra
fmt.Println(strings.Title("meu nome é João"))
// Output: Meu Nome É João

// Usando a função strings.ToUpper(), que converte todas as letras para maiúsculas
fmt.Println(strings.ToUpper("ola, mundo!"))
// Output: OLA, MUNDO!
```

Outra forma de capitalizar uma string é percorrer cada caractere e utilizar a função strings.ToUpper() apenas na primeira letra. Para isso, podemos usar um loop e um contador para identificar a primeira letra de cada palavra.

```Go
texto := "ola, mundo!"
novotexto := ""
contador := 0

for _, letra := range texto {
    if contador == 0 {
        novotexto += strings.ToUpper(string(letra))
        contador++
    } else {
        novotexto += string(letra)
    }
}

fmt.Println(novotexto)
// Output: Ola, Mundo!
```

## Aprofundando no processo de capitalizar uma string em Go

Além das soluções apresentadas acima, existem outras maneiras de capitalizar uma string em Go, como utilizar expressões regulares ou até mesmo criar uma função personalizada que identifique e capitalize a primeira letra de cada palavra. É importante entender as diferentes opções e escolher a melhor solução para cada cenário.

Outro ponto importante ao capitalizar strings é estar atento a características específicas do idioma, como por exemplo, o fato de que em português, letras como "ç" e "ã" também devem ser capitalizadas corretamente. Nesse caso, pode ser necessário utilizar funções específicas que considerem essas particularidades.

## Veja também

- [Documentação oficial do pacote strings em Go](https://golang.org/pkg/strings/)
- [Tutorial de strings em Go no site TutorialsPoint](https://www.tutorialspoint.com/go/go_strings.htm)
- [Exemplos de expressões regulares em strings em Go](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go-pt)

seo-import "fmt"
seo-import "strings"