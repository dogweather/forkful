---
title:    "PHP: Verificando se um diretório existe"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que Verificar se um Diretório Existe?

Verificar se um diretório existe é uma tarefa importante em programação, especialmente quando se trabalha com manipulação de arquivos e pastas. Ao realizar essa verificação, você pode garantir que seu código irá funcionar corretamente, evitando possíveis erros e falhas.

## Como Fazer

Para verificar se um diretório existe em PHP, é necessário utilizar a função `file_exists()`, que retorna `true` se o diretório existir e `false` se não existir. Além disso, também é possível utilizar a função `is_dir()`, que verifica se o diretório é realmente um diretório e não um arquivo.

Vejamos um exemplo prático:

```PHP
if (file_exists("meu_diretorio")) {
  echo "O diretório existe!";
} else {
  echo "O diretório não existe!";
}
```

Neste exemplo, a mensagem "O diretório existe!" será exibida se o diretório "meu_diretorio" existir no diretório atual. Caso contrário, a mensagem "O diretório não existe!" será exibida.

Também podemos utilizar a função `is_dir()` em conjunto com a função `file_exists()` para garantir que não estamos verificando um arquivo com o mesmo nome do diretório:

```PHP
if (file_exists("meu_diretorio")) {
  if (is_dir("meu_diretorio")) {
    echo "É um diretório!";
  } else {
    echo "Não é um diretório, é um arquivo!";
  }
} else {
  echo "O diretório não existe!";
}
```

Neste exemplo, a primeira condição verifica se o diretório "meu_diretorio" existe. Se existir, a segunda condição irá verificar se é realmente um diretório. Se sim, a mensagem "É um diretório!" será exibida. Se não, a mensagem "Não é um diretório, é um arquivo!" será exibida.

## Mergulho Profundo

Ao verificar se um diretório existe, é importante lembrar que, em algumas situações, o diretório pode existir, mas seu conteúdo não foi completamente carregado, o que pode causar problemas em sua manipulação. Por isso, é recomendado utilizar a função `realpath()` para convertê-lo em um caminho absoluto e garantir que todas as informações estejam carregadas corretamente.

Além disso, também é possível utilizar a função `scandir()` para listar os arquivos e subdiretórios dentro do diretório e realizar ações específicas em cada um deles.

## Veja Também

- [Função `file_exists()` no manual do PHP](https://www.php.net/manual/pt_BR/function.file-exists.php)
- [Função `is_dir()` no manual do PHP](https://www.php.net/manual/pt_BR/function.is-dir.php)
- [Função `realpath()` no manual do PHP](https://www.php.net/manual/pt_BR/function.realpath.php)
- [Função `scandir()` no manual do PHP](https://www.php.net/manual/pt_BR/function.scandir.php)