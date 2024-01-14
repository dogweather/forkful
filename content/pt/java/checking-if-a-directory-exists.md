---
title:    "Java: Verificando se um diretório existe"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Existem várias razões pelas quais um programador pode querer verificar se um diretório existe em Java. Isso pode ser útil para garantir que um determinado diretório esteja presente antes de prosseguir com a execução de um programa, para evitar erros e exceções, ou para lidar com situações específicas, como criar um diretório somente se ele não existir.

## Como verificar se um diretório existe em Java

Existem várias maneiras de verificar se um diretório existe em Java. Uma abordagem é utilizar a classe `File` e seu método `exists()`, que retorna um valor booleano indicando se o diretório especificado existe ou não.

```
Java
File diretorio = new File("caminho/do/diretorio");

if (diretorio.exists()) {
    System.out.println("O diretório existe!");
} else {
    System.out.println("O diretório não existe.");
}
```

Outra opção é utilizar a classe `Path` junto com o método `Files.exists()`, que também retorna um valor booleano.

```
Java
Path caminho = Paths.get("caminho/do/diretorio");

if (Files.exists(caminho)) {
    System.out.println("O diretório existe!");
} else {
    System.out.println("O diretório não existe.");
}
```

Além disso, também é possível utilizar a classe `Files` e seu método `isDirectory()` para verificar se um determinado caminho é um diretório ou não.

```
Java
Path caminho = Paths.get("caminho/do/diretorio");

if (Files.isDirectory(caminho)) {
    System.out.println("O caminho especificado é um diretório!");
} else {
    System.out.println("O caminho especificado não é um diretório.");
}
```

## Aprofundando-se na verificação de diretórios

Ao verificar se um diretório existe em Java, também é importante entender como lidar com possíveis exceções. Por exemplo, se o diretório especificado estiver em um local inacessível, o programa pode lançar uma `IOException`, que deve ser tratada adequadamente para evitar a interrupção do programa.

Além disso, é importante verificar se o diretório existe em tempo real, pois ele pode ser criado ou removido durante a execução do programa. Uma opção é utilizar o método `lastModified()` da classe `Files` para obter o horário da última modificação do diretório e fazer a verificação com base nisso.

## Veja também

- Documentação oficial da classe `File`: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- Documentação oficial da classe `Path`: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Path.html
- Documentação oficial da classe `Files`: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html