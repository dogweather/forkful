---
title:                "Usando expressões regulares"
html_title:           "Swift: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Sobre o que e por que?
Expressões regulares são uma forma de trabalhar com padrões de texto. Eles permitem que os programadores procurem e manipulem cadeias de caracteres com base em regras específicas. Os programadores usam expressões regulares para realizar tarefas como validação de dados e filtragem de informações.

## Como fazer:
Usando expressões regulares em Swift é simples e direto ao ponto. Primeiro, importe o módulo "Foundation" para ter acesso às funcionalidades de expressões regulares. Em seguida, crie uma expressão regular utilizando a sintaxe ```try NSRegularExpression(pattern: "padrão")```, onde "padrão" é o padrão que você deseja buscar. Por fim, use a função ```firstMatch(in: opcoes, range: NSRange)``` para obter o primeiro resultado que corresponde à sua expressão.

Por exemplo, se quisermos encontrar o padrão de um endereço de email em uma string, poderíamos usar o seguinte código:

```
import Foundation

let string = "Meu endereço de email é exemplo@email.com"

do {
    let padrao = try NSRegularExpression(pattern: "[a-z0-9]+@[a-z0-9]+\\.[a-z]")
    if let resultado = padrao.firstMatch(in: string, range: NSRange(string.startIndex..., in: string)) {
        print(string[Range(resultado.range, in: string)!])
    }
} catch let error {
    print("Erro: \(error.localizedDescription)")
}
```

Isso resultaria em "exemplo@email.com" sendo impresso na tela.

## Mergulho profundo:
As expressões regulares existem há muito tempo e são usadas em várias linguagens de programação. Elas foram criadas pelo matemático norte-americano Stephen Cole Kleene, na década de 1950, como uma forma de trabalhar com padrões em linguagens formais. Em Swift, há também alternativas ao uso de expressões regulares, como funções como ```contains``` e ```filter```, que podem ser mais simples em casos simples.

Nós também podemos usar opções adicionais ao criar uma expressão regular, como ignorar maiúsculas e minúsculas ou tratar metacaracteres de forma literal. Além disso, podemos usar capturas de grupo para obter partes específicas de uma string que correspondem ao nosso padrão.

## Veja também:
Para mais informações sobre como trabalhar com expressões regulares em Swift, você pode verificar a documentação oficial e o código-fonte do módulo Foundation. Além disso, existem ótimos tutoriais online que podem mostrar a você como utilizar expressões regulares em diferentes cenários.