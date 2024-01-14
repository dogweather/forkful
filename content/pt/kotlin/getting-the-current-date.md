---
title:    "Kotlin: Obtendo a data atual"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que obter a data atual?

Obter a data atual é uma tarefa comum na programação, especialmente em aplicações que lidam com informações baseadas em tempo, como em sistemas de reserva ou em redes sociais. Ter a data atual é importante para garantir que as informações estejam sempre atualizadas e precisas.

## Como fazer isso em Kotlin

Fazer uso da biblioteca padrão do Kotlin torna a obtenção da data atual uma tarefa fácil. Aqui está um exemplo simples de como obter a data atual em Kotlin:

```Kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// criando uma instância de LocalDateTime com a data e hora atual
val dataAtual = LocalDateTime.now()

// criando um objeto de formatação de data e hora
val formatador = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss")

// formatando a data atual com o objeto formatador
val dataFormatada = formatador.format(dataAtual)

// exibindo a data formatada
println(dataFormatada)
```

Output:
```
04/02/2021 20:30:15
```

## Mergulho Profundo

Por trás dos panos, o Kotlin utiliza as classes `LocalDateTime` e `DateTimeFormatter` da biblioteca padrão do Java para obter e formatar a data atual. O `LocalDateTime` representa uma data e hora específica, enquanto o `DateTimeFormatter` é responsável por formatar a data de acordo com o padrão especificado.

Outro detalhe importante é que a data e hora obtidas serão baseadas no fuso horário do sistema em que o código está sendo executado. Portanto, é importante se atentar a isso ao lidar com diferentes fusos horários em sua aplicação.

## Veja também

- [Documentação oficial do Kotlin sobre a biblioteca de data e hora](https://kotlinlang.org/docs/datetime.html)
- [Tutorial sobre como trabalhar com datas em Kotlin](https://www.baeldung.com/kotlin-datetime)