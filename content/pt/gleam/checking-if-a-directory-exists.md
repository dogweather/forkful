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

# Verificar Se um Diretório Existe em Gleam

Se você é um programador, provavelmente já se deparou com a necessidade de verificar se um diretório existe em algum momento. Mas o que significa verificar se um diretório existe e por que os programadores fazem isso?

## O que & Por quê?

Verificar se um diretório existe é um processo simples de determinar se um diretório específico existe no sistema de arquivos. Os programadores geralmente fazem isso para garantir que suas operações de leitura e gravação de arquivos não falhem por causa da ausência do diretório necessário.

## Como Fazer:

Para verificar se um diretório existe em Gleam, podemos usar a função `fs.Dir.exists/1`, passando o caminho do diretório desejado como argumento. Se o diretório existir, a função retornará `Ok` e, caso contrário, retornará `Err`.

```
Gleam ...
fs.Dir.exists("caminho/do/diretório")
|> Gleam.Result.inspect
```

A saída seria `Ok(true)` se o diretório existir ou `Ok(false)` se não existir.

## Mergulho Profundo

Antes do advento da programação funcional, os programadores de linguagens imperativas costumavam usar comandos como `mkdir` ou `dir /ad` para verificar a existência de um diretório. No entanto, em Gleam, podemos aproveitar a função `fs.Dir.exists/1` para uma solução mais concisa e elegante.

Para verificar a existência de um diretório em outros idiomas, os programadores podem recorrer a funções semelhantes, como `os.path.exists()` em Python ou `File.Exists()` no C#.

## Veja Também

- Documentação oficial da função `fs.Dir.exists/1` em Gleam: https://gleam.run/documentation/standard_library.html#fs.Dir.exists
- Exemplos adicionais e discussões sobre verificação de existência de diretório em Gleam: https://github.com/gleam-lang/gleam/issues/1289