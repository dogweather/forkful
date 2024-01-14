---
title:                "Java: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data para uma string?

Converter uma data para uma string é uma tarefa comum e importante em programação Java. Isso permite que o programador manipule e apresente a data em diferentes formatos, tornando a informação mais legível e adaptável às necessidades do usuário final.

## Como fazer:

Para converter uma data em uma string em Java, podemos utilizar o método formatDate() da classe SimpleDateFormat. Veja o exemplo abaixo:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class ConvertDataParaString {
    public static void main(String[] args) {
        Date data = new Date();

        SimpleDateFormat formatador = new SimpleDateFormat("dd/MM/yyyy");
        String dataFormatada = formatador.format(data);

        System.out.println("Data atual em formato de string: " + dataFormatada);
    }
}
```

Saída:

```
Data atual em formato de string: 23/09/2021
```

Podemos alterar o formato da string ao alterar os parâmetros passados para a classe SimpleDateFormat. Por exemplo, para mostrar a data e horário atual, podemos utilizar o formato "dd/MM/yyyy HH:mm:ss". Também é possível personalizar o formato adicionando símbolos como "dd de MMMM de yyyy" para mostrar a data como "23 de Setembro de 2021".

É importante lembrar que o método formatDate() da SimpleDateFormat é sensível ao local e fuso horário do sistema em que o código está sendo executado. Portanto, é recomendado especificar o local e o fuso horário desejado ao criar uma instância da classe.

## Mergulho profundo:

Além do método formatDate(), existem outras formas de converter uma data para uma string em Java. Uma delas é utilizando o método toString() da classe Date, que retorna a data no formato padrão. Outra opção é utilizar a classe DateTimeFormatter, introduzida no Java 8, que possui uma sintaxe mais limpa e permite formatações mais complexas.

É importante ter em mente que, ao trabalhar com datas, é essencial se atentar às diferenças entre datas locais e datas com fuso horário. Também é importante ter conhecimento sobre a classe Calendar, que é usada para manipular calendários, e a classe TimeZone, que representa fusos horários.

## Veja também:

- [Como trabalhar com datas em Java](https://www.devmedia.com.br/trabalhando-com-datas-em-java/27410)
- [Documentação da classe SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Documentação da classe DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Entendendo as classes Date, Calendar e TimeZone em Java](https://www.devmedia.com.br/calendar-em-java-trabalhando-com-datas-e-horas/32907)