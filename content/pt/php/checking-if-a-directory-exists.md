---
title:                "Verificando se um diretório existe"
html_title:           "PHP: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que
Às vezes, em um projeto de programação, precisamos verificar se um diretório existe antes de executar determinadas ações. Isso pode nos ajudar a evitar erros e garantir que nosso código funcione corretamente.

## Como Fazer
Podemos usar a função `file_exists()` do PHP para verificar se um diretório existe ou não. Veja o exemplo abaixo:
```PHP
<?php
    $diretorio = "imagens";

    if (file_exists($diretorio)) {
        echo "O diretório $diretorio existe.";
    } else {
        echo "O diretório $diretorio não existe.";
    }
```
Neste exemplo, utilizamos a variável `$diretorio` para armazenar o nome do diretório que queremos verificar. Em seguida, usamos a função `file_exists()` para verificar se ele existe ou não. Se existir, uma mensagem é exibida indicando isso. Caso contrário, outra mensagem é exibida.

## Mergulho Profundo
A função `file_exists()` retorna um valor booleano, ou seja, `true` se o diretório existir ou `false` se não existir. Portanto, podemos usá-la em condições `if` e `else` para executar diferentes ações dependendo do resultado.

Também é importante lembrar que a função `file_exists()` verifica apenas a existência do diretório em questão, não levando em conta se ele é legível ou gravável pelo usuário que está executando o código. Para verificar essas permissões, podemos usar outras funções como `is_readable()` e `is_writable()`.

## Veja Também
- Documentação oficial do PHP sobre a função `file_exists()`: https://www.php.net/manual/pt_BR/function.file-exists.php
- Como checar permissões em um diretório com PHP: https://www.php.net/manual/pt_BR/function.is-readable.php
- Verificando se um arquivo existe antes de ler/gravar: https://www.php.net/manual/pt_BR/function.file-exists.php