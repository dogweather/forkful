---
title:                "Unindo strings"
html_title:           "PHP: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/concatenating-strings.md"
---

{{< edit_this_page >}}

O que & Por quê?:

Concatenar strings é um processo de combinar duas ou mais strings para formar uma única string. Programadores o fazem para criar mensagens ou informações dinâmicas, que exigem a junção de diferentes partes em uma única string.

Como fazer:

```PHP
$string1 = "Olá";
$string2 = "mundo!";
echo $string1 . " " . $string2; // Output: Olá mundo!
```

Você também pode usar o operador de atribuição combinado (.=) para concatenar strings e reatribuir o valor à mesma variável. Isso pode ser útil quando você precisa adicionar mais partes à string em diferentes momentos no seu código.

```PHP
$string = "Olá";
$string .= " mundo!";
echo $string; // Output: Olá mundo!
```

Para concatenar mais de duas strings ao mesmo tempo, você pode usar a função `implode()` que recebe um delimitador e uma matriz de strings como parâmetros.

```PHP
$string1 = "Olá";
$string2 = "meu";
$string3 = "amigo!";
echo implode(" ", array($string1, $string2, $string3)); // Output: Olá meu amigo!
```

Mergulho Profundo:

Concatenação de strings é uma técnica comum e importante na programação, especialmente com PHP. Anteriormente, em versões mais antigas do PHP, a função `.` era usada para concatenar strings, mas agora é recomendado usar o operador `.=` ou a função `implode()`. Algumas linguagens de programação possuem concatenação de strings implícita, o que significa que você pode simplesmente adicionar duas strings sem qualquer operador ou função adicional.

Veja também:

- Documentação PHP sobre concatenação de strings: https://www.php.net/manual/pt_BR/language.operators.string.php
- Tutoriais de PHP para iniciantes: https://www.php.net/manual/pt_BR/tutorial.php#tutorial.learn