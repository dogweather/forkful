---
title:                "PHP: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando em PHP?

Se você é um desenvolvedor PHP e ainda não está familiarizado com a leitura de argumentos de linha de comando em seus scripts, este post é para você. Aprender a ler argumentos de linha de comando é uma habilidade muito útil que pode melhorar o seu fluxo de trabalho e tornar seu código mais dinâmico.

## Como ler argumentos de linha de comando em PHP

Para ler argumentos de linha de comando em PHP, usamos a função `getopt()`. Ela recebe dois parâmetros: uma string com as opções possíveis e um array com as opções de comando fornecidas pelo usuário. Vamos ver um exemplo:

```PHP
$options = getopt('a:b:c:d:');
var_dump($options);
```

Se executarmos o script acima com o comando `php meu_script.php -a 1 -b 2 -c 3`, obteremos a seguinte saída:

```
array(3) {
  ["a"]=>
  string(1) "1"
  ["b"]=>
  string(1) "2"
  ["c"]=>
  string(1) "3"
}
```

Neste exemplo, usamos a opção `getopt()` para definir quatro opções possíveis: `a`, `b`, `c` e `d`. Quando executamos o script, fornecemos três argumentos `-a`, `-b` e `-c`, que são armazenados em um array associativo.

Você também pode usar a opção `getopt()` para ler argumentos mais complexos, como argumentos com valores obrigatórios ou opcionais. Para isso, basta adicionar `:` após a letra da opção. Por exemplo, `getopt('a:b:')` espera que as opções `a` e `b` tenham valores obrigatórios.

## Aprofundando na leitura de argumentos de linha de comando em PHP

Além da função `getopt()`, existem outras maneiras de ler argumentos de linha de comando em PHP, como a variável superglobal `$_SERVER['argv']`. Além disso, a extensão do PHP `cli` oferece funcionalidades adicionais para trabalhar com argumentos de linha de comando.

Ao ler argumentos de linha de comando, é importante lembrar de validar os dados inseridos pelo usuário para garantir a segurança do seu script. Além disso, é uma boa prática fornecer mensagens de ajuda para o usuário, explicando como usar corretamente seus argumentos de linha de comando.

Aprender a ler argumentos de linha de comando em PHP pode ajudá-lo a criar scripts mais poderosos e flexíveis, tornando seu trabalho mais eficiente.

## Veja também
- [Documentação oficial do PHP para a função getopt()](https://www.php.net/manual/pt_BR/function.getenv.php)
- [Artigo sobre como ler argumentos de linha de comando em PHP](https://www.devmedia.com.br/como-ler-argumentos-de-linha-de-comando-em-php/29361)
- [Tutorial sobre a extensão cli do PHP](https://jorge-ferrer.com.br/usando-a-extensao-cli-do-php-para-criar-scripts-em-php/)