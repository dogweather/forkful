---
title:    "Java: Comparando duas datas"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar datas em Java é importante?

Muitas vezes, em projetos de programação, é necessário comparar datas para verificar quais eventos ocorreram antes ou depois de determinado momento ou simplesmente para verificar se duas datas são iguais. Saber como comparar datas em Java é uma habilidade importante para todo programador, pois permite que o código lide com informações temporais de forma eficiente e precisa.

## Como comparar datas em Java?

Existem várias maneiras de comparar datas em Java. A seguir, serão apresentados exemplos de código e saída de amostra usando dois objetos Date diferentes para demonstrar diferentes métodos de comparação.

### Usando o método compareTo()

Uma maneira de comparar duas datas é usando o método compareTo(). Ele retorna um valor inteiro que representa a diferença entre as duas datas no formato *ano-mês-dia*. Se o valor for positivo, significa que a primeira data é maior que a segunda. Se for negativo, a segunda data é maior. E se for zero, as duas datas são iguais.

```Java
Date data1 = new Date(2020, 1, 1); // 1 de janeiro de 2020
Date data2 = new Date(2020, 1, 15); // 15 de janeiro de 2020

int comparacao = data1.compareTo(data2);

System.out.println("A diferença entre as datas é: " + comparacao); // saída: -14
```

### Utilizando o método equals()

Outro jeito de comparar datas é usando o método equals() para verificar se elas são iguais. Ele retorna um valor booleano que indica se os dois objetos Date são iguais ou não.

```Java
Date data1 = new Date(2020, 5, 20); // 20 de maio de 2020
Date data2 = new Date(2020, 5, 20); // 20 de maio de 2020

boolean iguais = data1.equals(data2);

System.out.println("As datas são iguais? " + iguais); // saída: true
```

### Utilizando o método after() e before()

Outros métodos úteis para comparar datas são after() e before(), que verificam se uma data é posterior ou anterior a outra, respectivamente.

```Java
Date data1 = new Date(2020, 4, 10); // 10 de abril de 2020
Date data2 = new Date(2020, 5, 20); // 20 de maio de 2020

System.out.println("A primeira data é posterior à segunda? " + data1.after(data2)); // saída: false
System.out.println("A primeira data é anterior à segunda? " + data1.before(data2)); // saída: true
```

## Aprofundando-se na comparação de datas em Java

Java possui uma classe chamada Calendar, que é útil na comparação de datas, pois possui métodos para lidar com datas de forma mais detalhada, permitindo manipular informações como anos, meses, dias, horas, minutos e segundos.

Outro conceito importante é o formato de data. Java permite que se trabalhe com diferentes formatos, incluindo o padrão ISO 8601, que é muito utilizado em sistemas de comunicação.

## Veja também

- [Documentação oficial Java - Classe Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Manipulando datas em Java](https://www.devmedia.com.br/manipulando-datas-em-java/28657)
- [Tutorial de date e calendar em Java](https://www.programiz.com/java-programming/date-calendar)