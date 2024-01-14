---
title:    "PHP: Excluindo caracteres que correspondem a um padrão"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que deletar caracteres que correspondem a um padrão?

Às vezes, na programação, nos deparamos com a necessidade de deletar caracteres específicos que seguem um padrão em uma string ou array. Isso pode ser útil para diversas finalidades, como limpar dados ou validar inputs de usuários. Neste artigo, vamos aprender como fazer isso usando PHP.

## Como fazer

Para deletar caracteres que correspondem a um padrão em uma string, podemos usar a função `preg_replace()` do PHP. Esta função aceita três parâmetros: o padrão a ser procurado, o que será usado para substituir os caracteres correspondentes e a string em que a substituição será feita.

Vamos supor que temos a string "Olá, #mundo!". Se quisermos deletar todos os caracteres que começam com "#" (neste caso, apenas o "#"), podemos usar o seguinte código:

```PHP
$string = "Olá, #mundo!";
$padrao = '/#/';
$substituto = '';

$resultado = preg_replace($padrao, $substituto, $string);

echo $resultado; // Resultado: Olá, mundo!
```

Neste exemplo, estamos usando a expressão regular `/#/` como padrão, que significa "encontre todos os "#" na string". No segundo parâmetro, passamos uma string vazia para indicar que queremos deletar todos os caracteres que correspondem ao padrão.

Podemos também usar a função `preg_replace()` para deletar caracteres em uma array. Por exemplo, se tivermos uma array com os seguintes valores: "João", "Maria" e "Pedro", e quisermos deletar todas as letras "a":

```PHP
$array = array("João", "Maria", "Pedro");
$padrao = '/a/';
$substituto = '';

$resultado = preg_replace($padrao, $substituto, $array);

print_r($resultado); // Resultado: Array ( [0] => Jõo [1] => Mr[i] => Pedro )
```

As letras "a" em todos os nomes foram substituídas por uma string vazia, resultando em "João" se tornando "Jõo" e "Maria" se tornando "Mr[i]".

## Deep Dive

A expressão regular utilizada como padrão na função `preg_replace()` é formada por caracteres especiais que permitem uma busca mais precisa. Aqui estão alguns exemplos:

- `[a-z]`: Busca por qualquer letra minúscula entre "a" e "z".
- `[^a-z]`: Busca por qualquer caracter que NÃO seja uma letra minúscula entre "a" e "z".
- `\d`: Busca por qualquer dígito.
- `\s`: Busca por qualquer espaço em branco.

Existem muitos outros caracteres especiais que podem ser utilizados em expressões regulares. É importante lembrar que o padrão utilizado deve ser adequado à string ou array em que a substituição será feita.

## Veja Também

- [Documentação oficial do PHP sobre a função `preg_replace()`](https://www.php.net/manual/pt_BR/function.preg-replace.php)
- [Tutoriais sobre expressões regulares em PHP](https://www.guru99.com/php-regular-expressions.html)