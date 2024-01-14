---
title:    "Java: Convertendo uma data em uma string"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Converter uma data em uma string é uma tarefa comum em muitos projetos de programação Java. Isso permite que as datas sejam visualizadas de forma mais amigável e de acordo com o formato desejado.

## Como fazer

Para realizar a conversão de uma data em uma string em Java, utilizamos a classe `SimpleDateFormat`. Primeiro, precisamos criar uma instância dessa classe, definindo o formato que desejamos usar para a data. Por exemplo, se quisermos que a data seja exibida no formato "dd/MM/yyyy", podemos fazer da seguinte maneira:

```Java
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
```

Em seguida, usamos o método `format()` para converter a data em uma string, passando como parâmetro a data que desejamos converter. Por exemplo:

```Java
Date data = new Date(); // data atual
String dataFormatada = sdf.format(data);
System.out.println(dataFormatada); // saída: 28/10/2021
```

Podemos também escolher diferentes formatos, como "MM/dd/yyyy" ou "dd/MMM/yyyy", dependendo da necessidade do projeto. Além disso, também é possível especificar a localização, para que a data seja exibida de acordo com o idioma e região escolhidos.

## Aprofundando

Ao converter uma data em uma string, é importante entender a diferença entre os formatos de data e hora em Java. O formato tem o objetivo de determinar como uma data é apresentada, enquanto que a classe `Date` possui métodos para lidar com a lógica e manipulação de datas.

Além disso, é importante ter cuidado com o uso do método `format()`, pois ele pode gerar uma exceção se a data fornecida não estiver no formato especificado. Para evitar esse tipo de erro, é recomendado o uso de blocos `try-catch` ou o uso do método `parse()` para converter uma string em uma data.

## Veja também

- [Documentação oficial do Java sobre `SimpleDateFormat`](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Tutorial para converter uma data em Java](https://www.tutorialspoint.com/java/util/java_util_date.htm)