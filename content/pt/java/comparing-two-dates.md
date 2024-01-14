---
title:                "Java: Comparando duas datas"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que

Comparar datas pode ser uma tarefa comum para muitos programadores em Java. Isso pode ser feito por uma variedade de razões, como verificar a validade de uma data ou classificar objetos com base em suas datas.

## Como Fazer

Comparar duas datas em Java é simples, desde que você entenda como as datas são representadas no código. Vamos supor que você tenha duas datas como strings no formato "dd/mm/yyyy" e queira compará-las para ver qual é a mais recente. Você pode usar o método `SimpleDateFormat` para converter as strings em objetos `Date` e, em seguida, usar o método `compareTo` para compará-las.

```
// Criar duas strings contendo as datas
String data1 = "01/08/2021";
String data2 = "15/09/2021";

// Criar objetos SimpleDateFormat para converter as strings em Date
SimpleDateFormat formato = new SimpleDateFormat("dd/MM/yyyy");
Date dataObj1 = formato.parse(data1);
Date dataObj2 = formato.parse(data2);

// Comparar as datas usando o método compareTo
int resultado = dataObj1.compareTo(dataObj2);

// Verificar o resultado
if (resultado < 0) {
    System.out.println("A data1 é anterior à data2");
} else if (resultado > 0) {
    System.out.println("A data1 é posterior à data2");
} else {
    System.out.println("As datas são iguais");
}
```

O resultado será "A data1 é anterior à data2", pois 01/08/2021 vem antes de 15/09/2021. Você também pode usar os métodos `before` e `after` para comparar datas em relação a outras datas.

## Deep Dive

Existem algumas coisas importantes a serem consideradas ao comparar datas em Java. Primeiro, é importante lembrar que a classe `Date` não armazena informações de fuso horário, então, ao comparar datas de diferentes fusos horários, você pode obter resultados inesperados. Nesse caso, é melhor usar a classe `Calendar`, que leva em conta o fuso horário ao comparar datas.

Além disso, algumas questões relacionadas à precisão podem surgir ao comparar datas. A classe `Date` armazena a data e a hora em milissegundos, o que significa que pode haver pequenas diferenças na comparação de duas datas, mesmo que elas se refiram ao mesmo dia e horário. Para garantir uma comparação precisa, é recomendado usar o método `equals` ao invés de `compareTo`.

## Veja Também

- [Documentação oficial da classe Date em Java](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Comparando datas em Java de forma efetiva](https://www.baeldung.com/java-compare-dates)
- [Diferença entre o método compareTo e equals em Java](https://stackoverflow.com/questions/7683145/comparing-two-java-dates)