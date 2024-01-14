---
title:                "Java: Obtendo a data atual."
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual?

Às vezes, precisamos saber a data atual em nossos programas Java. Isso pode ser útil para rastrear a hora em que um determinado processo foi executado, criar um carimbo de data/hora em arquivos ou até mesmo para exibir a data para o usuário. Felizmente, obter a data atual em Java é bastante simples e pode ser realizado com apenas algumas linhas de código.

## Como fazer:

```Java
// Importando a biblioteca java.util.Date
import java.util.Date;

// Criando um objeto Date
Date data = new Date();

// Imprimindo a data atual
System.out.println(data);
```

A saída desse código será algo como:

```
Seg Jun 15 10:30:00 UTC 2020
```

Além disso, é possível formatar a data para exibi-la de forma mais legível para o usuário. Vamos ver como isso pode ser feito utilizando a classe SimpleDateFormat:

```Java
// Importando a biblioteca java.text.SimpleDateFormat
import java.text.SimpleDateFormat;

// Criando um objeto SimpleDateFormat com o formato desejado
SimpleDateFormat formato = new SimpleDateFormat("dd/MM/yyyy");

// Utilizando o objeto para formatar a data
String dataFormatada = formato.format(data);

// Imprimindo a data formatada
System.out.println(dataFormatada);
```

A saída agora será a data atual no formato especificado, por exemplo:

```
15/06/2020
```

## Profundando:

A classe Date em Java é utilizada para representar uma data e hora específica. Quando criamos um objeto Date sem passar nenhum parâmetro, ele é inicializado com a data e hora atual do sistema. No entanto, a classe Date não possui métodos para manipular ou extrair informações específicas, como dia, mês ou ano.

Por esse motivo, a partir do Java 8, foi introduzida a classe LocalDateTime, que possui métodos para lidar especificamente com datas e horas. Vamos ver como podemos utilizá-la para obter a data atual:

```Java
// Importando as bibliotecas java.time.LocalDateTime e java.time.format.DateTimeFormatter
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

// Criando um objeto LocalDateTime
LocalDateTime dataAtual = LocalDateTime.now();

// Utilizando um objeto DateTimeFormatter para formatar a data
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");

// Obtendo a data formatada
String dataFormatada = dataAtual.format(formatter);

// Imprimindo a data
System.out.println(dataFormatada);
```

A saída será a mesma do exemplo anterior, porém utilizando uma classe mais recente e específica para datas e horas.

## Veja também:

- [Documentação da classe Date em Java](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Documentação da classe LocalDateTime em Java](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Tutorial sobre formatação de datas em Java](https://www.baeldung.com/java-simpledateformat)