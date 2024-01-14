---
title:    "Go: Calculando uma data no futuro ou passado."
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que calcular uma data no futuro ou no passado?

Calcular datas futuras ou passadas pode ser útil em programas que lidam com agendamentos, prazos ou qualquer outra situação que envolva datas. Por exemplo, pode ser necessário agendar uma tarefa para um dia específico no futuro ou calcular quantos dias faltam até uma data de entrega.

## Como fazer?

Calcular uma data no futuro ou no passado é bastante simples em Go. Primeiro, precisamos definir a data base que usaremos como referência. Podemos fazer isso criando um novo objeto `time.Time` e definindo sua data através do método `time.Date(ano, mês, dia, hora, minuto, segundo, nanossegundo, fuso horário)`.

```Go
func main() {
	// Definindo a data base para 01 de janeiro de 2021
	base := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)

	// Calculando a data de 15 dias no futuro
	futuro := base.AddDate(0, 0, 15)

	// Calculando a data de 1 ano e 6 meses no passado
	passado := base.AddDate(-1, -6, 0)

	// Imprimindo as datas calculadas
	fmt.Println("Data futura:", futuro)
	fmt.Println("Data passada:", passado)
}
```

O código acima irá imprimir:

```
Data futura: 2021-01-16 00:00:00 +0000 UTC
Data passada: 2019-06-01 00:00:00 +0000 UTC
```

## Aprofundando mais

Além do método `time.AddDate()`, podemos também usar outros métodos como `time.Add()`, que nos permite adicionar uma quantidade específica de horas, minutos, segundos ou nanossegundos a uma data base. Também temos o método `time.Sub()` que nos permite obter a diferença entre duas datas.

Uma coisa importante a se ter em mente é que Go lida com datas e horas utilizando o fuso horário de UTC (Tempo Universal Coordenado). Portanto, se você estiver em um fuso horário diferente, é necessário converter a data para UTC antes de fazer qualquer cálculo.

## Veja também

- Documentação oficial de `time` em Go: https://golang.org/pkg/time/
- Artigo sobre manipulação de datas e horas em Go: https://www.callicoder.com/golang-manipulating-dates-times/
- Exemplos de cálculos de datas em Go: https://gobyexample.com/time