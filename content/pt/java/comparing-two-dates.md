---
title:    "Java: Comparando duas datas"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que

Comparar datas é uma tarefa comum em programação, especialmente quando se trabalha com dados que envolvem cronogramas ou prazos. Ao comparar duas datas, é possível determinar qual é mais recente ou se as datas são iguais.

## Como fazer

Para comparar duas datas em Java, é necessário utilizar a classe `LocalDate` e seu método `compareTo()`. Vamos ver um exemplo de código para comparar duas datas:

```Java
LocalDate data1 = LocalDate.of(2020, 9, 10);
LocalDate data2 = LocalDate.of(2020, 9, 15);

int comparacao = data1.compareTo(data2);
if(comparacao < 0) {
    System.out.println("Data 1 é anterior à Data 2");
} else if(comparacao > 0) {
    System.out.println("Data 1 é posterior à Data 2");
} else {
    System.out.println("As datas são iguais");
}
```

Neste exemplo, criamos duas variáveis do tipo `LocalDate`: uma com a data 10 de setembro de 2020 e outra com a data 15 de setembro de 2020. Em seguida, utilizamos o método `compareTo()` para comparar as duas datas e obter um valor de retorno. Esse valor é um inteiro que indica a relação entre as duas datas: se for negativo, significa que a primeira data é anterior à segunda, se for positivo, significa que é posterior, e se for zero, as datas são iguais. No exemplo, o resultado impresso será "Data 1 é anterior à Data 2".

## Aprofundando-se

Além de utilizar o método `compareTo()`, também é possível comparar datas utilizando outros métodos da classe `LocalDate`, como `isBefore()`, `isAfter()` e `isEqual()`. Também é importante lembrar que a classe `LocalDate` possui opções de formatação para exibir as datas de diferentes maneiras.

## Veja também

- [Documentação oficial do Java sobre a classe LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial da DevMedia sobre como comparar datas em Java](https://www.devmedia.com.br/comparando-datas-em-java-com-localdate/37972)