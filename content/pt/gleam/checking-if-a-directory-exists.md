---
title:                "Gleam: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Se você está trabalhando com o Gleam, é provável que esteja criando uma aplicação que precisa lidar com arquivos e diretórios. Saber como verificar se um diretório existe é uma habilidade importante para garantir que sua aplicação funcione corretamente. Felizmente, o Gleam possui uma função simples que permite fazer essa verificação de forma fácil e eficiente. Continue lendo para descobrir como fazer isso!

## Como Fazer

Para verificar se um diretório existe no Gleam, você precisará usar a função `Std.Path.dir_exists/1`. Essa função aceita um caminho de string como argumento e retorna um booleano `true` se o diretório existir ou `false` se não existir. Vamos dar uma olhada em um exemplo:

```Gleam
import Std.Path

dir_name = "exemplo/diretorio"
result = Std.Path.dir_exists(dir_name)

io.format("O diretório ~s existe? ~b", [dir_name, result])
```

No exemplo acima, estamos importando o módulo `Std.Path` para ter acesso à função `dir_exists`. Em seguida, declaramos o caminho do diretório que queremos verificar e passamos ele como argumento para a função `dir_exists`. Finalmente, usamos a função `io.format` para imprimir uma mensagem informando se o diretório existe ou não.

A saída deste programa seria:

```
O diretório exemplo/diretorio existe? true
```

## Aprofundando

Por trás das cenas, a função `Std.Path.dir_exists/1` está usando a função do sistema operacional `Sys.is_dir/1` para verificar a existência do diretório. Isso significa que a função `dir_exists` retornará `true` mesmo se o caminho passado apontar para um arquivo com o mesmo nome. Além disso, lembre-se de que essa função só verificará se o diretório existe no caminho absoluto. Se você quiser verificar se um diretório existe em um caminho relativo, primeiro terá que convertê-lo para um caminho absoluto usando a função `Std.Path.relative_to_absolute/1`.

## Veja Também

- [Documentação oficial do Gleam sobre a função `Std.Path.dir_exists/1`](https://gleam.run/core/`Std.Path.html#dir_exists:1`)
- [Guia de consulta rápida do Gleam com a função `Std.Path.dir_exists/1`](https://gleam.run/book/stdlib/files.html#checking-for-the-existence-of-a-directory)