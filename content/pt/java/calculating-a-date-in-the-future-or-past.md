---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Java: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

---

# Calculando Datas Futuras ou Passadas em Java

---

## O Que & Por Quê?

Calcular uma data no futuro ou no passado é uma operação que manipula uma data para retornar uma nova, acrescentando ou subtraindo dias, meses ou anos. Os programadores fazem isso para realizar tarefas como programar lembretes, fazer agendamentos e encontrar diferenças entre datas.

---

## Como Fazer:

Vamos calcular uma data futura usando a classe `java.time.LocalDate` do Java 8 e superior:

```java
import java.time.LocalDate;
import java.time.Period;

public class Main{
    public static void main(String[] args) {
        LocalDate hoje = LocalDate.now();
        Period periodo = Period.ofDays(7); // substitua 7 pelo número de dias que quer adicionar
        LocalDate dataFutura = hoje.plus(periodo);
        System.out.println("In 7 days, the date will be " + dataFutura);
    }
}
```

Está é a saída do código:

```bash
In 7 days, the date will be 2022-10-15
```

---

## Mergulho Profundo

(1) A necessidade de calcular datas futuras ou passadas é tão antiga quanto a própria programação. Com as linguagens modernas como o Java, essa tarefa tornou-se muito mais simples e intuitiva.

(2) Existem várias alternativas para calcular datas futuras ou passadas, como o uso da classe `java.util.Calendar` ou `java.util.Date`, mas essas classes são menos intuitivas e mais propensas a erros.

(3) O método `plus()` que usamos acima adiciona o período especificado à nossa data. É importante notar que este método não altera a data original, mas retorna uma nova instância de `LocalDate`. Isto porque as classes de data e hora da API java.time são imutáveis.

---

## Veja Também

- API oficial do Java para `java.time`: [<https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html>]

- Documentação completa para a classe `java.time.LocalDate`: [<https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html>]

- Tutorial da Oracle sobre a API java.time: [<https://docs.oracle.com/javase/tutorial/datetime/index.html>]