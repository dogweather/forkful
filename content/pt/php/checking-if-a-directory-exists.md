---
title:                "PHP: Verificando se um diretório existe"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

### Por que verificar se um diretório existe?

Verificar se um diretório existe é um passo importante ao realizar operações de manipulação de arquivos em um projeto de programação PHP. Isso permite que você garanta que o caminho do diretório fornecido é válido antes de manipulá-lo, evitando erros no seu código.

### Como fazer

```PHP
// Verificando se o diretório existe
if (is_dir('/caminho/do/diretorio')) {
  // Código a ser executado caso o diretório exista
  echo "O diretório existe!";
} else {
  // Código a ser executado caso o diretório não exista
  echo "O diretório não existe!";
}
```

No código acima, usamos a função `is_dir()` para checar se o diretório especificado existe. Caso exista, o bloco de código dentro do `if` é executado, caso contrário, o bloco dentro do `else` é executado.

### Aprofundando-se

Além da função `is_dir()`, também é possível usar outras funções para verificar a existência de diretórios em um projeto PHP, como `file_exists()` e `is_readable()`. Além disso, é importante lembrar que é possível trabalhar com caminhos absolutos ou relativos ao verificar a existência de um diretório.

### Veja também

- [Documentação oficial do PHP sobre a função `is_dir()`](https://www.php.net/manual/pt_BR/function.is-dir.php)
- [Post do blog "PHP para iniciantes: manipulação de arquivos e diretórios"](https://blog.geekhunter.com.br/php-para-iniciantes-manipulacao-de-arquivos-e-diretorios/)
- [Videoaula sobre manipulação de arquivos e diretórios em PHP](https://www.youtube.com/watch?v=-A7Hl--IGJc)