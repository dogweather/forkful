---
title:    "Java: Verificando se um diretório existe"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Verificar se um diretório existe é uma tarefa importante ao escrever um código Java relacionado a arquivos e diretórios. Isso garante que o código possa lidar adequadamente com situações em que o diretório não existe ou pode estar oculto.

## Como fazer:

Para verificar se um diretório existe em Java, podemos usar o método `exists()` da classe `File`. Este método retorna um valor booleano indicando se o diretório existe ou não. Aqui está um exemplo de código que utiliza o método `exists()`:

```
import java.io.File;

public class Main {
  public static void main(String[] args) {
    // Definindo o caminho do diretório que queremos verificar
    String diretorio = "C:\\Users\\Usuario\\Downloads";

    // Criando um objeto File com o diretório fornecido
    File arquivo = new File(diretorio);

    // Verificando se o diretório existe
    if (arquivo.exists()) {
      System.out.println("O diretório existe!");
    } else {
      System.out.println("O diretório não existe!");
    }
  }
}
```

O código acima irá imprimir "O diretório existe!" se o diretório especificado realmente existir.

## Mais detalhes:

Existem alguns pontos importantes a serem lembrados ao usar o método `exists()`:

- Este método não verifica se o caminho fornecido é um diretório ou um arquivo. Ele simplesmente verifica se há um item (seja arquivo ou diretório) com o caminho especificado.
- O método `exists()` retorna `false` se o caminho fornecido apontar para um arquivo oculto ou inacessível.
- Se o diretório fornecido não existir, este método também retornará `false`.

## Veja também:

Aqui estão alguns recursos adicionais relacionados a verificação de diretórios em Java:

- [Documentação do método exists()](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#exists--)
- [Tutorial de Java File IO](https://www.javatpoint.com/java-file-io)
- [Artigo sobre como trabalhar com arquivos e diretórios em Java](https://www.baeldung.com/java-file-io)