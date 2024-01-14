---
title:    "PHP: Verificando se um diretório existe."
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe

Verificar se um diretório existe é uma etapa importante em muitos projetos de programação PHP. Isso permite que o código tenha um comportamento condicional, podendo lidar com cenários em que o diretório pode não existir.

## Como fazer

Para verificar se um diretório existe em PHP, podemos usar a função `is_dir()`. Essa função retorna `true` se o diretório existir e `false` caso contrário. Veja um exemplo de código abaixo:

```PHP
$directory = "/caminho/para/diretorio"; // substitua pelo diretório que deseja checar

if (is_dir($directory)) {
    echo "O diretório existe";
} else {
    echo "O diretório não existe";
}
```

O código acima irá verificar se o diretório especificado na variável `$directory` existe e imprimir uma mensagem de acordo. Caso o diretório não exista, o código entrará no `else` e imprimirá "O diretório não existe".

## Deep Dive

Além da função `is_dir()`, existem outras maneiras de verificar se um diretório existe em PHP. Por exemplo, podemos usar a função `file_exists()` que também retorna `true` ou `false` dependendo se o diretório existe ou não. Outra opção é usar a classe `DirectoryIterator`, que permite iterar sobre os arquivos e diretórios em um determinado caminho.

## Veja também

Aqui estão alguns links úteis para saber mais sobre como verificar se um diretório existe em PHP:

- [Documentação oficial do PHP sobre a função `is_dir()`](https://www.php.net/manual/pt_BR/function.is-dir.php)
- [Tutorial do PHPJabbers sobre como verificar se um diretório existe](https://www.phpjabbers.com/check-if-directory-exists-php-php13.html)
- [Exemplos práticos de como verificar se um diretório existe em PHP](https://www.geeksforgeeks.org/php-check-if-a-directory-exists/)