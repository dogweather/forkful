---
title:                "Java: Convertendo uma data em uma string"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

A conversão de uma data para uma string é uma tarefa comum na programação Java. Isso pode ser necessário para exibir a data em um formato específico, armazenar em um banco de dados ou até mesmo enviar por meio de uma API. Aprender a converter uma data para uma string é uma habilidade essencial para qualquer programador Java.

## Como Fazer

Para converter uma data para uma string, usamos o método `format()` da classe `java.text.SimpleDateFormat`. Este método aceita dois parâmetros: um padrão de formato e a data que queremos converter.

```
import java.text.SimpleDateFormat;
import java.util.Date;

public class ConversaoDataString {
    public static void main(String[] args) {
        // cria um objeto de data
        Date data = new Date();

        // define o padrão de formato 
        SimpleDateFormat formato = new SimpleDateFormat("dd/MM/yyyy");

        // converte a data para uma string
        String dataString = formato.format(data);

        // exibe o resultado
        System.out.println("Data convertida para string: " + dataString);
    }
}
```

O padrão de formato utilizado neste exemplo é "dd/mm/yyyy", que significa dia/mês/ano. Outros padrões estão disponíveis para exibir a data em diferentes formatos, como o formato de 24 horas, exibição do dia da semana, entre outros. Ao executar o código acima, a saída será algo como "Data convertida para string: 08/09/2021", mostrando a data atual no formato desejado.

## Aprofundamento

Quando lidamos com datas em Java, é importante entender o conceito de "época" ou "epoch". A epoch é um ponto de referência a partir do qual podemos medir o tempo. Em Java, a epoch é definida como 01/01/1970 00:00:00 UTC. Todas as datas são contadas a partir deste momento, sendo que valores positivos são usados para datas após a epoch e valores negativos para datas anteriores.

Além disso, é importante ter cuidado com o uso do método `toString()` da classe `java.util.Date`. Este método retorna uma representação da data e hora atual em um formato específico, mas pode variar entre plataformas e configurações de idioma. Portanto, é recomendado utilizar o método `format()` para garantir um formato consistente em todas as situações.

## Veja também

- Documentação oficial do método `format()`: https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html#format-java.util.Date-
- Guia sobre datas e tempos em Java: https://www.baeldung.com/java-dates
- Vídeo tutorial sobre conversão de data em Java: https://www.youtube.com/watch?v=uC2IYcSOb68