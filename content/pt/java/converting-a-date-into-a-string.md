---
title:                "Convertendo uma data em uma sequência de caracteres."
html_title:           "Java: Convertendo uma data em uma sequência de caracteres."
simple_title:         "Convertendo uma data em uma sequência de caracteres."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que
Converter uma data em uma string é uma tarefa comum em muitos aplicativos Java. Ao convertê-la, os desenvolvedores podem exibir a data em um formato específico ou armazená-la em um banco de dados.

## Como Fazer
Para converter uma data em uma string no Java, você precisa seguir os seguintes passos:

1. Crie um objeto do tipo `Date` e inicialize com a data desejada.
```
Java Date data = new Date();
```
2. Crie um objeto do tipo `SimpleDateFormat` e especifique o formato desejado para a string. Por exemplo, "dd/MM/yyyy" para exibir a data no formato dia/mês/ano.
```
SimpleDateFormat formato = new SimpleDateFormat("dd/MM/yyyy");
```
3. Chame o método `format()` do objeto `SimpleDateFormat`, passando o objeto `Date` como parâmetro, para obter a data em formato de string.
```
String dataString = formato.format(data);
```
4. A data convertida será armazenada na variável `dataString` e você pode usá-la conforme necessário no seu código.

Esse é um exemplo simples e básico de como converter uma data em uma string no Java. Lembre-se de que existem várias opções de formatação disponíveis e você pode escolher aquela que melhor se adapta às suas necessidades.

## Aprofundamento
O Java possui várias classes para trabalhar com datas e horários, como `Date`, `Calendar` e `LocalDate`. A classe `Date` é usada principalmente para representar uma data específica, enquanto a classe `Calendar` fornece mais funcionalidades, como cálculos de datas e manipulação de fusos horários.

No entanto, a partir do Java 8, foi introduzida a classe `LocalDate` que fornece uma maneira mais fácil e intuitiva de manipular datas e horários. Ela também resolve alguns problemas de inconsistência presentes nas outras classes. Para converter uma data em uma string usando `LocalDate`, você pode fazer o seguinte:
```
// Cria um objeto LocalDate com a data desejada
LocalDate data = LocalDate.of(2020, 5, 15);

// Formata a data em uma string usando o padrão "dd/MM/yyyy"
String dataString = data.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"));
```
A classe `LocalDate` também possui vários métodos para manipular a data, como `plusDays()` para adicionar dias, `isLeapYear()` para verificar se o ano é bissexto e muito mais.

Além disso, com a classe `DateTimeFormatter`, você pode personalizar ainda mais o formato da string, adicionando informações como horas, minutos, segundos e fusos horários.

## Veja também
- [Java Date Formatting Guide](https://www.baeldung.com/java-format-datetime)
- [Oracle Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial de Data e Hora no Java](https://www.tutorialspoint.com/java/java_date_time.htm)