---
title:                "Verificando se um diretório existe"
html_title:           "Bash: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Hã? Verificar se um diretório existe é algo que nos permite confirmar a existência de um diretório numa determinada localização no sistema de arquivos. E por que fazemos isto? Para evitar erros e criar diretórios se necessário.

## Como Fazer: 

Para verificar se um diretório existe em PHP, usamos a função `is_dir()`, que retorna TRUE se o diretório existe e FALSE caso contrário.

```PHP
<?php
$dir = '/caminho/para/o/diretorio';

if (is_dir($dir)) {
    echo "$dir existe.";
} else {
    echo "$dir não existe.";
}
?>
```

Se você executar o código e o diretório existir, a saída será:

```PHP
/caminho/para/o/diretorio existe.
```

E se o diretório não existir, a saída será:

```PHP
/caminho/para/o/diretorio não existe.
```

## Mergulhando Mais Fundo: 

**Contexto histórico:** A função `is_dir()` foi introduzida no PHP 4, lançado em 2000, e permaneceu um meio eficaz de verificar a existência de diretórios desde então.

**Alternativas:** É possível também executar um comando shell usando a função `shell_exec()`, mas tem que nos assegurarmos de que o usuário PHP tenha as permissões necessárias para executar comandos shell.

**Detalhes de implementação:** Por baixo do capô, a função `is_dir()` no PHP usa a função `stat` do sistema operacional para verificar a existência do diretório. Esta função é muito rápida e eficaz, mas a sua disponibilidade pode depender do sistema operacional e das permissões de acesso ao sistema de arquivos.

## Veja Também:

* [Referência oficial da função PHP is_dir()](https://www.php.net/manual/pt_BR/function.is_dir)
* [Fórum Stackoverflow sobre checar se diretório existe em PHP](https://stackoverflow.com/questions/1707801/making-a-new-directory-in-php)
* [Tutorial detalhado sobre manipulação de diretório em PHP](https://www.w3schools.com/php/php_ref_directory.asp)