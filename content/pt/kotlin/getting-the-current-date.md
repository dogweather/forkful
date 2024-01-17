---
title:                "Obtendo a data atual"
html_title:           "Kotlin: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# O que & Por quê?
Pegar a data atual é uma tarefa comum para programadores em diferentes linguagens de programação, incluindo Kotlin. Isso permite que seus programas tenham informações precisas sobre o tempo, além de permitir a criação de lógicas baseadas em datas, como exibir mensagens específicas em feriados.

# Como fazer:
Em Kotlin, para obter a data atual, podemos usar a função "now()" da classe "LocalDate". Veja o exemplo abaixo:

```Kotlin
val currentDate = LocalDate.now()
println(currentDate)
```

## Resultado:
```
2021-10-02
```

# Profundando:
## Contexto histórico:
Antigamente, a obtenção da data atual podia ser uma tarefa mais complexa, exigindo o uso de bibliotecas externas ou até mesmo fazer cálculos matemáticos com registros de tempo. Com o avanço da tecnologia e o surgimento de novas linguagens, essa tarefa ficou muito mais simples e acessível.

## Alternativas:
Além do método apresentado acima, é possível obter a data atual usando outras classes e funções em Kotlin, como a classe "Calendar" e a função "Date()". No entanto, a função "now()" é considerada a melhor opção por ser mais simples e ter uma sintaxe mais limpa.

## Detalhes de implementação:
A classe "LocalDate" faz parte da biblioteca padrão do Kotlin e é usada para representar uma data sem tempo ou fuso horário. Além disso, a função "now()" retorna a data atual com base no fuso horário do sistema em que o programa está sendo executado.

# Veja também:
- [Documentação oficial do Kotlin sobre a classe LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)
- [Tutorial sobre como trabalhar com datas em Kotlin](https://www.baeldung.com/kotlin/dates)