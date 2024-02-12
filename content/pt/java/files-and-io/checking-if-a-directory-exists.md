---
title:                "Verificando se um diretório existe"
date:                  2024-02-03T19:08:02.850869-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verificando se um diretório existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
Verificar se um diretório existe em Java é uma tarefa fundamental que envolve a verificação da presença de um diretório no sistema de arquivos antes de ler, escrever ou realizar qualquer operação que exija sua existência. Isso é crucial para evitar erros ou exceções em programas que interagem com o sistema de arquivos, garantindo uma execução mais suave e uma melhor experiência do usuário.

## Como fazer:
Em Java, existem várias maneiras de verificar se um diretório existe, principalmente usando as classes `java.nio.file.Files` e `java.io.File`.

**Usando `java.nio.file.Files`**:

Esta é a abordagem recomendada nas versões mais recentes do Java.

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DiretorioExiste {
    public static void main(String[] args) {
        // Especifique o caminho do diretório aqui
        String caminhoDoDiretorio = "caminho/para/diretório";

        // Verificando se o diretório existe
        if (Files.exists(Paths.get(caminhoDoDiretorio))) {
            System.out.println("O diretório existe.");
        } else {
            System.out.println("O diretório não existe.");
        }
    }
}
```
**Saída de Exemplo**:
```
O diretório existe.
```
Ou 
```
O diretório não existe.
```

**Usando `java.io.File`**:

Embora `java.nio.file.Files` seja recomendado, a classe mais antiga `java.io.File` também pode ser usada.

```java
import java.io.File;

public class DiretorioExisteLegado {
    public static void main(String[] args) {
        // Especifique o caminho do diretório aqui
        String caminhoDoDiretorio = "caminho/para/diretório";

        // Criando um objeto File
        File diretorio = new File(caminhoDoDiretorio);

        // Verificando se o diretório existe
        if (diretorio.exists() && diretorio.isDirectory()) {
            System.out.println("O diretório existe.");
        } else {
            System.out.println("O diretório não existe.");
        }
    }
}
```
**Saída de Exemplo**:
```
O diretório existe.
```
Ou
```
O diretório não existe.
```

**Usando Bibliotecas de Terceiros**:

Embora a biblioteca padrão do Java geralmente seja suficiente para essa tarefa, bibliotecas de terceiros como o Apache Commons IO oferecem utilitários adicionais de manipulação de arquivos que podem ser úteis em aplicações mais complexas.

**Apache Commons IO**:

Primeiro, adicione a dependência do Apache Commons IO ao seu projeto. Depois, você pode usar suas funcionalidades para verificar a existência de um diretório.

```java
// Assumindo que o Apache Commons IO foi adicionado ao projeto

import org.apache.commons.io.FileUtils;

public class DiretorioExisteCommons {
    public static void main(String[] args) {
        // Especifique o caminho do diretório aqui
        String caminhoDoDiretorio = "caminho/para/diretório";

        // Usando FileUtils para verificar
        boolean diretorioExiste = FileUtils.directoryContains(new File(caminhoDoDiretorio), null);

        if (diretorioExiste) {
            System.out.println("O diretório existe.");
        } else {
            System.out.println("O diretório não existe.");
        }
    }
}
```

**Nota**: `FileUtils.directoryContains` verifica se um diretório contém um arquivo específico, mas ao passar `null` como segundo argumento, você pode usá-lo para verificar a existência do diretório. Tenha cuidado, pois essa pode não ser a forma mais direta ou intencionada de usar o método.

**Saída de Exemplo**:
```
O diretório existe.
```
Ou
```
O diretório não existe.
```
