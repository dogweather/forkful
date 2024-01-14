---
title:                "Gleam: Verificando se um diretório existe"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Verificar se um diretório existe é um passo importante ao lidar com arquivos e pastas em um programa Gleam. É uma maneira de garantir que o código funcione corretamente e evite erros ao tentar acessar um diretório inexistente.

## Como fazer

A verificação de diretório pode ser facilmente feita usando a função "file::exists" do módulo "Builtins". Esta função retorna um valor booleano indicando se o diretório existe ou não. Veja um exemplo de uso abaixo:

```
Gleam
import Builtins

fn check_directory(directory: String) {
  let exists = Builtins.file::exists(directory)

  case exists {
    true -> io.println("O diretório " ++ directory ++ " existe.")
    false -> io.println("O diretório " ++ directory ++ " não existe.")
  }
}

// Exemplo de chamada da função
check_directory("caminho/para/o/diretorio")
```

Saída:

```
O diretório caminho/para/o/diretorio existe.
```

## Mergulho Profundo

Além de verificar se o diretório existe, também é possível verificar permissões de acesso e outras propriedades do diretório usando funções como "file::has_permissions" e "file::get_attributes". Estas funções são úteis para um controle mais específico do diretório e podem ajudar a evitar possíveis erros no código.

## Veja também

- [Documentação do módulo Builtins](https://gleam.run/modules/builtin.html)
- [Exemplos de uso da função file::exists](https://github.com/gleam-lang/gleam/blob/master/examples/file/file.gleam)