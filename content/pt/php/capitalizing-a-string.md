---
title:    "PHP: Capitalizando uma string"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Capitalizar uma string é um processo essencial em muitos programas PHP. Isso envolve tornar a primeira letra de cada palavra em uma string maiúscula. Isso pode ser útil para formatar nomes, títulos ou qualquer outra informação em que a capitalização é necessária.

## Como fazer

Existem algumas maneiras de capitalizar uma string em PHP. Vamos dar uma olhada em algumas delas:

```PHP
// Transformando a primeira letra em maiúscula
$string = "exemplo de string";
$string = ucfirst($string);
echo $string;

// Saída: Exemplo de string

// Transformando a primeira letra de cada palavra em maiúscula
$string = "exemplo de string";
$string = ucwords($string);
echo $string;

// Saída: Exemplo De String
```

É importante notar que esses métodos não alteram a string original, mas retornam uma nova string capitalizada. Se você deseja alterar a string original, pode usar a função `strtoupper()` para transformar todas as letras em maiúsculas.

## Mergulho profundo

Existem outras funções em PHP que podem ser úteis ao capitalizar uma string. Por exemplo, a função `mb_convert_case()` pode ser usada para lidar com caracteres multibyte, como acentos. Você também pode usar expressões regulares para capitalizar apenas as letras após os espaços em branco.

Além disso, para facilitar o processo de capitalização, você pode criar sua própria função personalizada para capitalizar de acordo com suas necessidades específicas.

## Veja também

- [Funções de manipulação de strings do PHP](https://www.php.net/manual/pt_BR/ref.strings.php)
- [Documentação sobre expressões regulares em PHP](https://www.php.net/manual/pt_BR/book.pcre.php)