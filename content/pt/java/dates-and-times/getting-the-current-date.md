---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:44.652994-07:00
description: "Como fazer: Java oferece v\xE1rias maneiras de obter a data atual, usando\
  \ tanto a antiga classe `java.util.Date` quanto o mais novo pacote `java.time`\u2026"
lastmod: '2024-03-13T22:44:46.466342-06:00'
model: gpt-4-0125-preview
summary: "Java oferece v\xE1rias maneiras de obter a data atual, usando tanto a antiga\
  \ classe `java.util.Date` quanto o mais novo pacote `java.time` (introduzido no\
  \ Java 8), que \xE9 mais vers\xE1til e intuitivo."
title: Obtendo a data atual
weight: 29
---

## Como fazer:
Java oferece várias maneiras de obter a data atual, usando tanto a antiga classe `java.util.Date` quanto o mais novo pacote `java.time` (introduzido no Java 8), que é mais versátil e intuitivo.

### Usando `java.time.LocalDate`
```java
import java.time.LocalDate;

public class ExemploDataAtual {
    public static void main(String[] args) {
        LocalDate dataAtual = LocalDate.now();
        System.out.println(dataAtual); // Exemplo de saída: 2023-04-01
    }
}
```

### Usando `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class ExemploDataAtual {
    public static void main(String[] args) {
        LocalDateTime dataHoraAtual = LocalDateTime.now();
        System.out.println(dataHoraAtual); // Exemplo de saída: 2023-04-01T12:34:56.789
    }
}
```

### Usando `java.util.Date` (Legado)
```java
import java.util.Date;

public class ExemploDataAtual {
    public static void main(String[] args) {
        Date dataAtual = new Date();
        System.out.println(dataAtual); // Exemplo de saída: Sat Apr 01 12:34:56 BST 2023
    }
}
```

### Utilizando uma Biblioteca de Terceiros: Joda-Time
Antes do Java 8, Joda-Time era o padrão de facto para data e hora em Java. Se você está trabalhando em sistemas legados ou tem preferência pelo Joda-Time, aqui está como você pode usá-lo para obter a data atual:
```java
import org.joda.time.LocalDate;

public class ExemploDataAtual {
    public static void main(String[] args) {
        LocalDate dataAtual = LocalDate.now();
        System.out.println(dataAtual); // Exemplo de saída: 2023-04-01
    }
}
```
**Nota:** Embora `java.util.Date` e Joda-Time ainda sejam usados, o pacote `java.time` é recomendado para novos projetos devido à sua imutabilidade e API abrangente para manuseio de datas e horas.
