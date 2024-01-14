---
title:                "Java: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Java?

Comparar datas pode ser uma tarefa comum em diferentes cenários de programação, seja para verificar se uma data é maior ou menor que outra, ou para calcular a diferença entre duas datas. Neste artigo, vamos explorar como comparar duas datas em Java e entender a lógica por trás disso.

## Como fazer a comparação

A classe Date da linguagem Java possui o método `compareTo()` que permite comparar duas datas. Este método retorna um valor inteiro que representa a diferença entre as duas datas. Se o valor for menor que zero, isso significa que a data1 é anterior à data2. Se o valor for maior que zero, a data1 será posterior à data2. E se o valor for igual a zero, as duas datas são iguais.

Veja um exemplo de código com a utilização do `compareTo()`:

```Java
Date data1 = new Date("2021-01-15");
Date data2 = new Date("2021-01-20");

int resultado = data1.compareTo(data2);

if (resultado < 0) {
  System.out.println("A data1 é anterior à data2");
} else if (resultado > 0) {
  System.out.println("A data1 é posterior à data2");
} else {
  System.out.println("As datas são iguais");
}
```

O código acima irá imprimir "A data1 é anterior à data2", pois a data1 é 15/01/2021 e a data2 é 20/01/2021.

Outra forma de comparar datas é usando o método `before()` ou `after()`, que retornam um valor booleano indicando se a data é anterior ou posterior à outra.

```Java
if (data1.before(data2)) { // retorna true se a data1 for anterior à data2
  System.out.println("A data1 é anterior à data2");
} else if (data1.after(data2)) { // retorna true se a data1 for posterior à data2
  System.out.println("A data1 é posterior à data2");
} else {
  System.out.println("As datas são iguais");
}
```

## Aprofundando mais na comparação de datas

Quando usamos apenas o método `compareTo()`, o resultado da comparação pode ser influenciado por fatores externos, como o fuso horário do sistema. Para garantir uma comparação precisa, é recomendado utilizar a classe `Calendar` junto com o método `compareTo()`.

Além disso, o Java 8 introduziu as classes `LocalDate`, `LocalTime` e `LocalDateTime`, que permitem uma manipulação mais fácil de datas e horas sem a necessidade de lidar com fusos horários.

Também é importante considerar o formato das datas utilizadas, pois a comparação pode gerar resultados errados se os formatos forem diferentes. É recomendado usar o formato `yyyy-MM-dd` (ano-mês-dia) para evitar confusões.

Com essas dicas, você pode se aprofundar no assunto e explorar diferentes formas de comparar datas em Java.

## Veja também

- [Tutorial oficial da Oracle sobre comparação de datas em Java](https://docs.oracle.com/javase/tutorial/datetime/iso/date.html)
- [Documentação completa da classe Date em Java](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Artigo da DevMedia sobre comparação de datas em Java](https://www.devmedia.com.br/comparacao-de-datas-em-java/15734)