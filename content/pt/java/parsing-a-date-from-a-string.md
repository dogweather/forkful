---
title:                "Analisando uma data de uma string"
html_title:           "Java: Analisando uma data de uma string"
simple_title:         "Analisando uma data de uma string"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# O que e por que?

A conversão de uma data de string é quando um programador transforma uma data em formato de texto em um objeto de data utilizável em seu código. Os programadores fazem isso para poder manipular e calcular datas de forma mais eficiente em seus programas.

# Como fazer:

Para converter uma data de string em Java, é necessário utilizar a classe "SimpleDateFormat" e seu método "parse", passando como parâmetro a string com a data e o formato esperado. Por exemplo:

```Java 
SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
Date date = dateFormat.parse("01/05/2021");
```
A saída será um objeto Date com a data 01/05/2021.

# Profundando:

A conversão de uma data de string é uma prática antiga na programação, pois as linguagens de programação não possuem um tipo de dado específico para datas. Além disso, existem outras formas de fazer essa conversão, como utilizar bibliotecas de terceiros que oferecem funcionalidades mais avançadas.

Implementar a conversão de data em Java pode ser um pouco trabalhoso, pois é necessário especificar o formato da data e lidar com possíveis erros de formato. No entanto, a classe SimpleDateFormat possui diversas opções de formatação que podem facilitar esse processo.

# Veja também:

- Documentação oficial da classe SimpleDateFormat: https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
- Tutoriais sobre conversão de data em Java: https://www.baeldung.com/java-date-to-string-conversion
- Outras formas de trabalhar com datas em Java: https://www.baeldung.com/java-dates