---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:10.590064-07:00
description: "Como fazer: O pacote New I/O (NIO) do Java (`java.nio.file`) fornece\
  \ uma abordagem mais vers\xE1til para lidar com arquivos. Aqui est\xE1 uma maneira\
  \ simplista\u2026"
lastmod: '2024-03-13T22:44:46.474132-06:00'
model: gpt-4-0125-preview
summary: "O pacote New I/O (NIO) do Java (`java.nio.file`) fornece uma abordagem mais\
  \ vers\xE1til para lidar com arquivos."
title: Escrevendo um arquivo de texto
weight: 24
---

## Como fazer:


### Usando `java.nio.file` (Biblioteca Padrão)
O pacote New I/O (NIO) do Java (`java.nio.file`) fornece uma abordagem mais versátil para lidar com arquivos. Aqui está uma maneira simplista de escrever em um arquivo usando `Files.write()`:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> linhas = Arrays.asList("Linha 1", "Linha 2", "Linha 3");
        try {
            Files.write(Paths.get("exemplo.txt"), linhas);
            System.out.println("Arquivo escrito com sucesso!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Saída:

```
Arquivo escrito com sucesso!
```

### Usando `java.io` (Biblioteca Padrão)
Para uma abordagem mais tradicional, `java.io.FileWriter` é uma boa escolha para escrever arquivos de texto de maneira simples:

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("exemplo.txt")) {
            writer.write("Olá, Mundo!\n");
            writer.append("Esta é outra linha.");
            System.out.println("Arquivo escrito com sucesso!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Saída:

```
Arquivo escrito com sucesso!
```

### Usando Apache Commons IO
A biblioteca Apache Commons IO simplifica muitas operações, incluindo a escrita de arquivos. Veja como escrever em um arquivo usando `FileUtils.writeStringToFile()`:

Primeiro, adicione a dependência ao seu projeto. Se estiver usando Maven, inclua:

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- Verifique a última versão -->
</dependency>
```

Depois, use o seguinte código para escrever texto em um arquivo:

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("exemplo.txt"), "Este é um texto escrito usando Commons IO.", "UTF-8");
            System.out.println("Arquivo escrito com sucesso!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

Saída:

```
Arquivo escrito com sucesso!
```
