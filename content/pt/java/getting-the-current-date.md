---
title:    "Java: Obtendo a data atual."
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual?

Em programação, muitas vezes precisamos trabalhar com datas e horas. Portanto, é importante saber como obter a data atual em nosso código Java. A data atual pode ser útil para fins de registro, comparação ou simplesmente para mostrar a data em um formato desejado. Felizmente, a linguagem Java facilita muito a obtenção da data atual.

## Como fazer:

Primeiro, precisamos importar a classe "java.util.Date" para o nosso código:

```Java
import java.util.Date;
```

Em seguida, podemos criar um objeto do tipo Date e atribuí-lo à data atual usando o construtor padrão:

```Java
Date dataAtual = new Date();
```

Podemos imprimir a data atual em milissegundos usando o método "getTime()":

```Java
System.out.println(dataAtual.getTime());
```

Isso nos dará um número representando a data atual em milissegundos a partir de 1º de janeiro de 1970. Além disso, podemos obter a data atual em um formato legível usando o método "toString()":

```Java
System.out.println(dataAtual.toString());
```

Este comando irá imprimir a data atual em uma forma como "Wed Sep 08 12:53:39 BRT 2021". Mas se quisermos mostrar a data em um formato específico, podemos usar a classe "SimpleDateFormat" e seu método "format()" para formatar nossa data atual:

```Java
import java.text.SimpleDateFormat;

SimpleDateFormat formatador = new SimpleDateFormat("dd/MM/yyyy");
System.out.println(formatador.format(dataAtual));
```

Isso imprimirá a data atual no formato "08/09/2021". Podemos alterar o formato de acordo com nossas necessidades, basta consultar a documentação da classe "SimpleDateFormat" para conhecer os códigos de formatação disponíveis.

## Deep Dive:

A partir do Java 8, foi introduzida a classe "LocalDateTime", que nos permite trabalhar com a data e hora atuais mais facilmente. Além disso, também foi adicionado o método "now()" à classe "LocalDateTime" que retorna a data e hora atuais sem a necessidade de criar um objeto. Podemos usá-lo da seguinte forma:

```Java
import java.time.LocalDateTime;

LocalDateTime dataAtual = LocalDateTime.now();
System.out.println(dataAtual);
```

Este comando imprimirá a data e hora atuais em um formato como "2021-09-08T14:05:48.932". Podemos usar métodos como "getDayOfMonth()" ou "getYear()" para obter apenas o dia ou o ano atual, respectivamente. Além disso, também podemos usar essa classe para manipular datas e horas de maneira mais precisa e eficiente.

## Veja também:

- Documentação oficial do Java sobre a classe Date: https://docs.oracle.com/javase/8/docs/api/java/util/Date.html
- Documentação oficial do Java sobre a classe LocalDateTime: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html
- Exemplos práticos de uso da classe Date: https://www.baeldung.com/java-date
- Tutorial sobre a classe LocalDateTime: https://www.baeldung.com/java-8-date-time-intro