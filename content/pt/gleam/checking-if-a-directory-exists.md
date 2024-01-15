---
title:                "Verificando se um diretório existe"
html_title:           "Gleam: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que fazer uma verificação de existência de diretório?

Se você está trabalhando com arquivos em seu programa, é importante ter certeza de que o diretório no qual deseja salvar ou acessar seus arquivos existe. Isso pode evitar erros e garantir que seu código funcione corretamente.

## Como fazer uma verificação de existência de diretório em Gleam

Para verificar se um diretório existe em Gleam, podemos utilizar a função `os.dir_exists`, que retorna um booleano indicando se o diretório existe ou não. Veja um exemplo de código abaixo:

```
import gleam/os

let dir_exists = os.dir_exists("./meu_diretorio")
```

Isso verifica se o diretório "meu_diretorio" existe no local atual do programa. Se o diretório existir, a variável `dir_exists` será atribuída como `true`, caso contrário, será `false`.

Podemos então adicionar uma lógica em nosso código para lidar com a existência ou não do diretório da seguinte forma:

```
if dir_exists {
  io.println("O diretório existe!")
} else {
  io.println("O diretório não existe!")
}
```

Dessa forma, podemos lidar com o diretório de acordo com a nossa necessidade.

## Mergulho Profundo

Além da função `os.dir_exists`, Gleam também possui outras funções relacionadas à manipulação de diretórios, como `os.dir_create` para criar um novo diretório e `os.dir_delete` para excluir um diretório existente. Além disso, é possível utilizar a função `os.cwd` para obter o diretório atual do programa.

Outro ponto importante a ser mencionado é que a função `os.dir_exists` pode lançar uma exceção em caso de problemas de permissão ou outros erros. Portanto, é sempre uma boa prática envolver sua chamada em um bloco de tratamento de erros.

## Veja também

- Documentação oficial da biblioteca Gleam (em Inglês): <https://gleam.run/documentation>
- Recurso de manipulação de arquivos e diretórios no Gleam (em Inglês): <https://gleam.run/modules/os>
- Exemplo de código de verificação de existência de diretório em Gleam: <https://github.com/gleam-lang/gleam_stdlib/blob/master/lib/test/os/test_dir_exists.gleam>