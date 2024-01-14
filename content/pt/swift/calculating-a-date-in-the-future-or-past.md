---
title:                "Swift: Calculando uma data no futuro ou passado"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado pode ser um problema comum para muitos programadores em Swift. Seja para agendar eventos ou gerar informações de data dinâmicas, entender como calcular datas pode ser útil em muitos projetos.

## Como Fazer

Existem várias maneiras de calcular datas no Swift, dependendo de suas necessidades específicas. Aqui estão algumas opções que podem ajudá-lo a obter o resultado desejado:

```Swift
// Obtendo a data atual
let currentDate = Date()

// Adicionando 1 dia à data atual
let futureDate = Calendar.current.date(byAdding: .day, value: 1, to: currentDate)

// Subtraindo 1 semana da data atual
let pastDate = Calendar.current.date(byAdding: .weekOfYear, value: -1, to: currentDate)

// Obtendo o dia, mês e ano de uma data específica
let components = Calendar.current.dateComponents([.day, .month, .year], from: futureDate)

// Personalizando um formato de data
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let formattedDate = dateFormatter.string(from: futureDate!)
```

Com esses exemplos, você pode personalizar suas próprias funções para calcular datas no seu projeto Swift.

## Mergulho Profundo

Ao calcular datas, é importante ter em mente a utilização do fuso horário correto, pois isso pode afetar os resultados. Além disso, a classe `Calendar` oferece muitas opções e métodos úteis para manipular datas, como adicionar ou subtrair valores específicos, extrair componentes, comparar datas, entre outros. É sempre bom revisar a documentação oficial da Apple para entender todas as possibilidades.

## Veja Também

- [Documentação Oficial da Apple sobre a classe Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Tutoriais sobre formatação de datas em Swift](https://www.hackingwithswift.com/example-code/system/how-to-format-dates-with-dateformatter)
- [Funções personalizadas de cálculo de datas em Swift](https://www.hackingwithswift.com/example-code/language/how-to-write-the-result-of-your-function-to-the-screen)