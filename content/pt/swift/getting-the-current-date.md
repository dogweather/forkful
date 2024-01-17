---
title:                "Obtendo a data atual"
html_title:           "Swift: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que e Por Que?
A data atual é uma função importante em programação que permite aos desenvolvedores obter a data e a hora atuais em seus códigos. Isso é útil para uma variedade de tarefas, como registrar eventos, agendar tarefas e exibir informações relevantes para o usuário.

## Como Fazer:
Existem várias maneiras de obter a data atual em Swift, mas a forma mais simples é usando o objeto Date e em seguida, formatando-o para atender às suas necessidades. Aqui estão dois exemplos de como fazer isso:

### Exemplo 1:
```Swift
let dataAtual = Date() // Cria um objeto Date com a data e hora atuais
let formato = DateFormatter()
formato.locale = Locale(identifier: "pt_BR") // Define o formato da data/hora
formato.dateFormat = "dd/MM/yyyy - HH:mm" // Define o formato da data/hora
let dataFormatada = formato.string(from: dataAtual) // Formata a data atual
print(dataFormatada) // Saída: "07/07/2021 - 16:30"
```

Você também pode usar uma sintaxe mais simplificada para obter a data atual em um determinado formato:

### Exemplo 2:
```Swift
let formato = DateFormatter()
formato.locale = Locale(identifier: "pt_BR")
formato.dateFormat = "dd/MM/yyyy"
let dataAtual = formato.string(from: Date())
print(dataAtual) // Saída: "07/07/2021"
```

## Aprofundando um Pouco Mais:
A obtenção da data atual tem sido uma necessidade constante na programação desde o início. Anteriormente, era necessário usar funções mais complicadas e fórmulas matemáticas para obter a data correta. No entanto, com o avanço da tecnologia e o uso de objetos Date, é possível obter a data atual de forma rápida e fácil.

É importante observar que existem outras formas de obter a data atual em Swift, como por meio do objeto Calendar, que permite manipular facilmente datas e horários.

## Veja Também:
- Documentação oficial da Apple sobre o objeto Date: https://developer.apple.com/documentation/foundation/date
- Tutorial sobre como utilizar o objeto DateFormatter: https://www.hackingwithswift.com/example-code/system/how-to-convert-dates-and-times-to-a-string-using-dateformatter
- Fonte gratuita para formatos de data e hora: https://nsdateformatter.com/