---
title:    "PHP: Lendo argumentos da linha de comando"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade essencial para qualquer programador PHP, pois permite que você crie programas interativos que possam receber entradas do usuário em tempo real. Além disso, isso também ajuda a personalizar a execução do seu código de acordo com as necessidades específicas de cada usuário.

## Como Fazer

Para ler argumentos da linha de comando em PHP, você pode utilizar a função `getopt()` que retorna os argumentos fornecidos na chamada do programa em um array associativo. Por exemplo:

```PHP
<?php
$opcoes = getopt("a:b:"); //leitura dos argumentos -a e -b
echo "Argumento de a: " . $opcoes['a'] . "\n";
echo "Argumento de b: " . $opcoes['b'] . "\n";
?>
```

Se executarmos este código com a seguinte entrada `php leitura_argumentos.php -a teste -b 123`, teremos o seguinte resultado:

```
Argumento de a: teste
Argumento de b: 123
```

## Explorando mais a fundo

Além de utilizar a função `getopt()`, você também pode acessar diretamente os argumentos da linha de comando através do array global `$argv`. Este array contém todos os argumentos fornecidos na chamada do programa, incluindo o próprio nome do arquivo PHP. Seguindo o exemplo anterior, podemos acessar os argumentos da seguinte forma:

```PHP
<?php
echo "Argumento de a: " . $argv[1] . "\n";
echo "Argumento de b: " . $argv[2] . "\n";
?>
```

Ou seja, `$argv[1]` representa o primeiro argumento após o nome do arquivo e assim por diante.

## Veja também

- [Documentação oficial do PHP sobre a função `getopt()`](https://www.php.net/manual/pt_BR/function.getopt.php)
- [Documentação oficial do PHP sobre o array global `$argv`](https://www.php.net/manual/pt_BR/reserved.variables.argv.php)
- [Vídeo tutorial sobre leitura de argumentos da linha de comando em PHP](https://www.youtube.com/watch?v=K-E_Y6JSUAA)