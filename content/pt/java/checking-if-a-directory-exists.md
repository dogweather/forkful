---
title:                "Java: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Muitas vezes, em programação, é necessário verificar se um determinado diretório existe antes de realizar alguma ação no sistema de arquivos. Isso é especialmente importante em casos em que o diretório é criado dinamicamente ou pode ter sido excluído pelo usuário. Verificar a existência de um diretório pode evitar erros e falhas no seu código, garantindo uma melhor experiência para os usuários do seu programa.

## Como verificar se um diretório existe em Java

Verificar se um diretório existe em Java é uma tarefa simples. Basta utilizar a classe `File` e o método `exists()`, que retornará um valor booleano indicando se o diretório existe ou não. Veja um exemplo de código abaixo:

```Java
// Cria um objeto File para o diretório que queremos verificar
File diretorio = new File("/caminho/do/diretorio");

// Verifica se o diretório existe
if (diretorio.exists()) {
    System.out.println("O diretório existe!");
} else {
    System.out.println("O diretório não existe!");
}
```

A saída desse código será "O diretório existe!", caso o diretório exista, ou "O diretório não existe!", caso contrário.

## Mergulho profundo

Ao utilizar o método `exists()`, é importante lembrar que ele também retornará `true` se o caminho fornecido se referir a um arquivo em vez de um diretório. Para verificar especificamente se um diretório existe, podemos usar o método `isDirectory()`. Veja um exemplo:

```Java
// Cria um objeto File para o diretório que queremos verificar
File diretorio = new File("/caminho/do/diretorio");

// Verifica se o diretório existe e é realmente um diretório
if (diretorio.exists() && diretorio.isDirectory()) {
    System.out.println("O diretório existe!");
} else {
    System.out.println("O diretório não existe ou não é um diretório válido!");
}
```

Além disso, o método `exists()` também pode retornar `true` para links simbólicos que apontam para um diretório existente. Se quisermos verificar se o diretório de destino do link simbólico realmente existe, podemos usar o método `getCanonicalFile()` em conjunto com o `exists()`, como mostrado no exemplo abaixo:

```Java
// Cria um objeto File para o link simbólico que queremos verificar
File linkSimbolico = new File("/caminho/para/o/link/simbolico");

// Verifica se o diretório de destino do link simbólico existe
if (linkSimbolico.exists() && linkSimbolico.getCanonicalFile().exists()) {
    System.out.println("O diretório existe!");
} else {
    System.out.println("O diretório não existe ou o link simbólico não é válido!");
}
```

## Veja também

- [Documentação oficial da classe `File` em Java](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Tutorial completo sobre manipulação de arquivos e diretórios em Java (em inglês)](https://www.baeldung.com/java-io-vs-nio)