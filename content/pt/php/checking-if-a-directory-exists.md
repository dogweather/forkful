---
title:                "PHP: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Por que verificar se um diretório existe?

Às vezes, em nosso código PHP, é necessário verificar se um diretório existe antes de executar certas ações. Isso pode ser útil para garantir que nosso script possa acessar e manipular os arquivos dentro do diretório corretamente, evitando erros ou exceções inesperadas.

# Como verificar se um diretório existe

Podemos verificar a existência de um diretório usando a função `is_dir()` do PHP. Esta função recebe um parâmetro, o caminho do diretório que desejamos verificar, e retorna `true` se o diretório existir ou `false` se não existir.

```
<?php
if (is_dir('caminho/para/diretorio') === true) {
    echo "O diretório existe!";
} else {
    echo "O diretório não existe.";
}
```

Se o diretório existir, a saída do código acima será "O diretório existe!". No entanto, se o diretório não existir, a saída será "O diretório não existe.".

# Aprofunde-se na verificação da existência de diretórios

Além do `is_dir()`, o PHP possui outras funções úteis para trabalhar com diretórios, como `mkdir()` para criar um novo diretório e `rmdir()` para remover um diretório existente.

Podemos também usar a função `scandir()` para obter uma lista de arquivos e diretórios dentro de um diretório específico.

É importante lembrar que a verificação da existência de um diretório é uma etapa importante no processo de manipulação de arquivos, pois nos permite garantir que nosso código funcione corretamente.

# Veja também

- Documentação oficial do PHP para `is_dir()`: https://www.php.net/manual/pt_BR/function.is-dir.php
- Tutorial sobre manipulação de arquivos no PHP: https://www.tutorialspoint.com/php/php_file_management.htm
- Vídeo explicativo sobre `is_dir()` e outras funções de diretórios no PHP: https://www.youtube.com/watch?v=NG0nTjBbM_s