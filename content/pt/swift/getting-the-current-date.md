---
title:                "Obtendo a data atual."
html_title:           "Swift: Obtendo a data atual."
simple_title:         "Obtendo a data atual."
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, precisamos saber a data atual em nossos projetos de programação. Pode ser para criar um sistema de login que registra a última vez que um usuário acessou o aplicativo ou para apresentar a data em que um post foi publicado em um blog. Felizmente, a linguagem de programação Swift possui recursos integrados que nos permitem obter a data atual facilmente.

## Como fazer

Para obter a data atual em Swift, podemos usar a classe `Date` e o formato de data `DateFormatter`. Veja um exemplo abaixo:

```Swift
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let formattedDate = dateFormatter.string(from: date)

print(formattedDate) // Saída: 22/10/2021
```

Primeiro, criamos uma instância da classe `Date` que representa a data atual. Em seguida, usamos o `DateFormatter` para formatar a data de acordo com o formato desejado, neste caso, "dd/MM/yyyy". Depois, usamos o método `string(from: date)` para converter a data em uma string formatada. Por fim, imprimimos a data formatada na saída.

Outra opção é utilizar a função `Date()` diretamente na formatação, como no exemplo abaixo:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let formattedDate = dateFormatter.string(from: Date())

print(formattedDate) // Saída: 22/10/2021
```

Isso nos dá o mesmo resultado que o primeiro exemplo, mas com uma linha a menos de código.

## Profundidade

A classe `Date` possui uma variedade de métodos e propriedades para nos ajudar a lidar com datas e horas. Além do formato de data, também podemos definir o fuso horário e o calendário específico que queremos utilizar. Por exemplo, podemos usar o calendário islâmico em vez do calendário gregoriano padrão.

Além disso, podemos fazer operações matemáticas com datas, como adicionar ou subtrair dias, semanas, meses ou anos. Ainda, podemos comparar duas datas e verificar se são iguais ou se uma é anterior ou posterior à outra.

## Veja também

- [Documentação oficial do Swift sobre a classe Date](https://developer.apple.com/documentation/foundation/date)
- [Tutorial da Hacking with Swift sobre como trabalhar com datas em Swift](https://www.hackingwithswift.com/articles/106/how-to-use-dates-and-dateformatters-in-swift)
- [Vídeo do canal Swiftly sobre como usar datas em Swift](https://www.youtube.com/watch?v=Yz4lLx31udk)