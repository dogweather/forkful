---
title:    "Java: Creating um arquivo temporário"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Java?

Criar um arquivo temporário pode ser uma necessidade comum em alguns projetos de programação em Java. Isso pode ser útil quando você precisa armazenar dados temporariamente ou quando precisa criar, manipular e excluir arquivos temporários durante a execução do seu código.

## Como criar um arquivo temporário em Java

Aqui estão alguns exemplos de como criar um arquivo temporário em Java, utilizando a classe `java.nio.file.Files` e o método `createTempFile()`:

```Java
// Importando as classes necessárias
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

// Definindo o prefixo e sufixo do nome do arquivo temporário
String prefixo = "temp";
String sufixo = ".txt";

// Criando o arquivo temporário
try {
    File arquivo = Files.createTempFile(prefixo, sufixo).toFile();

    // Imprimindo o nome do arquivo temporário criado
    System.out.println("Nome do arquivo temporário: " + arquivo.getName());
} catch (IOException e) {
    e.printStackTrace();
}
```

### Saída de exemplo:
```
Nome do arquivo temporário: temp923827060450272162.txt
```

Também é possível definir um diretório específico para o arquivo temporário ser criado, utilizando o método `createTempFile(prefixo, sufixo, diretório)`.

## Mergulho mais profundo

Ao criar um arquivo temporário em Java, é importante entender como ele é gerenciado pelo sistema operacional e pela máquina virtual Java (JVM). Quando um arquivo temporário é criado, ele é armazenado em um local especificado pelo sistema operacional e fica marcado para exclusão assim que o programa em Java é finalizado ou quando o arquivo é fechado.

Caso o arquivo não seja excluído corretamente (por exemplo, em caso de falha do programa), o sistema operacional se encarregará de excluí-lo durante a limpeza de arquivos temporários.

## Veja também

- [Documentação oficial do Java sobre a classe `java.nio.file.Files`](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html)
- [Tutorial da Oracle sobre a criação de arquivos temporários em Java](https://docs.oracle.com/javase/tutorial/essential/io/file.html)