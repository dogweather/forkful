---
title:    "Go: Obtendo a data atual"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual em Go?

Ao criar um programa em Go, é importante ser capaz de manipular datas e horários de forma eficiente. Isso pode ser útil para tarefas como agendar tarefas, registrar eventos e calcular o tempo de execução do programa. A obtenção da data atual é uma das habilidades básicas mais importantes em Go e pode ser feita de maneira simples e direta.

## Como fazer:

Para obter a data atual em Go, podemos usar a função `time.Now()`. Esta função retorna um valor do tipo `time.Time`, que contém informações como data, hora e fuso horário. Veja abaixo um exemplo de código:

```
dataAtual := time.Now()
fmt.Println(dataAtual)
```

O código acima irá imprimir a data atual no seguinte formato:

```
2021-01-01 15:30:45.123456789 +0000 UTC m=+0.000000001
```

Podemos também formatar a data de acordo com nossas preferências, utilizando o método `Format()` da struct `time.Time`. Por exemplo, para exibir apenas a data no formato "dd/mm/aaaa", podemos fazer o seguinte:

```
dataFormatada := dataAtual.Format("02/01/2006")
fmt.Println(dataFormatada)
```

Isso irá imprimir "01/01/2021" no console. É importante notar que as letras utilizadas para a formatação seguem um padrão específico, e as referências para cada parte da data devem ser exatamente como no exemplo acima.

## Aprofundando-se:

Além de obter a data atual, também é possível fazer operações matemáticas e comparações com datas em Go. Por exemplo, podemos adicionar ou subtrair períodos de tempo ao valor `time.Time` utilizando o método `Add()`.

```
dataFutura := dataAtual.AddDate(1, 0, 0) // Adiciona 1 ano à data atual
```

Também podemos comparar duas datas utilizando os operadores `==`, `!=`, `<`, `>`, `<=` e `>=`, ou calcular a diferença entre elas em termos de segundos, minutos, horas, dias, etc.

```
diferenca := dataFutura.Sub(dataAtual) // Calcula a diferença em termos de tempo
```

É importante lembrar que, ao trabalharmos com datas e horários em Go, é necessário levar em consideração questões como fuso horário e formatos de data/hora utilizados em diferentes locais.

## Veja também:

- [Documentação oficial do pacote "time" em Go](https://golang.org/pkg/time/)
- [Tutorial sobre manipulação de datas em Go](https://www.callicoder.com/golang-datetime-tutorial/)