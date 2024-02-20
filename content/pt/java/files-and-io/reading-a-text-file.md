---
date: 2024-01-20 17:54:33.548968-07:00
description: "Ler um arquivo de texto em Java \xE9 pegar a informa\xE7\xE3o armazenada\
  \ num arquivo no seu computador e us\xE1-la dentro do seu programa. Fazemos isso\
  \ porque, \xE0s\u2026"
lastmod: 2024-02-19 22:05:05.513152
model: gpt-4-1106-preview
summary: "Ler um arquivo de texto em Java \xE9 pegar a informa\xE7\xE3o armazenada\
  \ num arquivo no seu computador e us\xE1-la dentro do seu programa. Fazemos isso\
  \ porque, \xE0s\u2026"
title: Lendo um arquivo de texto
---

{{< edit_this_page >}}

## O Que & Por Quê?
Ler um arquivo de texto em Java é pegar a informação armazenada num arquivo no seu computador e usá-la dentro do seu programa. Fazemos isso porque, às vezes, os dados que precisamos estão guardados nesse formato, ou queremos permitir que usuários interajam com nossos programas por meio de arquivos.

## Como Fazer:
```Java
import java.nio.file.*;
import java.io.*;

public class LeitorDeTexto {
    public static void main(String[] args) {
        Path caminho = Paths.get("caminho/para/arquivo.txt");
        
        try {
            String conteudo = Files.readString(caminho);
            System.out.println("Conteúdo do arquivo:");
            System.out.println(conteudo);
        } catch (IOException e) {
            System.out.println("Erro ao ler o arquivo: " + e.getMessage());
        }
    }
}
```

Saída de exemplo:
```
Conteúdo do arquivo:
Olá, mundo!
Aqui estão os dados do seu arquivo.
```

## Mergulho Profundo
Antigamente, ler arquivos em Java era uma tarefa mais verbosa, pois exigia manipulação manual de `InputStreams`, `FileReaders` e `BufferedReaders`. Agora, com `java.nio.file.Files`, podemos fazer isso em uma linha com `readString()`.

Existem alternativas. Uma delas é usar o `Scanner` para ler linha por linha, útil para arquivos grandes ou para parsing personalizado. `BufferedReader` também é uma boa escolha quando precisamos de mais controle sobre a leitura.

Falando em implementação, é importante lembrar de tratar as exceções. Arquivos podem não existir, estar inacessíveis ou corrompidos, e o tratamento de erros é crítico para evitar que o programa quebre.

## Veja Também
- [Documentação da Classe Files](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html)
- [Lendo arquivos com BufferedReader](https://www.baeldung.com/java-buffered-reader)
- [Uso do Scanner para leitura de arquivos](https://www.baeldung.com/java-scanner)newline
