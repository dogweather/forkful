---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Criar um arquivo temporário é a conjuração de um arquivo de armazenamento de dados temporário no seu sistema de computador. Programadores frequentemente fazem isso para armazenar informações temporárias como logs de sistema, arquivos de cache, arquivos de processamento em lote, etc.

## Como Fazer:

No Kotlin, usamos a função `createTempFile` da biblioteca padrão para criar um arquivo temporário. Veja abaixo um exemplo simples:

```Kotlin
import java.io.File

fun main() { 
    val tempFile = File.createTempFile("meuArquivoTemp", ".tmp") 
    println("Arquivo temporário criado em: ${tempFile.absolutePath}") 
}
```

Ao executar este código, o Kotlin cria um arquivo temporário e imprime o seu caminho absoluto.

## Aprofundamento

Historicamente, a criação de arquivos temporários era uma prática comum para lidar com dados intermediários em sistemas computacionais com restrições de memória. Embora os computadores modernos tenham menos restrição, a utilidade desta prática prevalece.

Existem alternativas à função `createTempFile`, dependendo das necessidades específicas. Por exemplo, poderia usar `Files.createTempDirectory()` para criar um diretório temporário.

Os detalhes da implementação da função `createTempFile` mostram que o Kotlin usa internamente a função `java.io.File.createTempFile` do Java. Portanto, o arquivo temporário é criado no local especificado pela propriedade do sistema `java.io.tmpdir`.

## Veja Também

2. Documentação do Java sobre criação de arquivos temporários: [https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String)](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String))