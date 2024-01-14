---
title:                "PHP: Maiúscula de uma cadeia de caracteres"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Capitalizar uma string é um processo importante em programação para garantir que o texto esteja formatado corretamente e legível para o usuário final. Além disso, a capitalização adequada é essencial para fins de pesquisa e classificação em bases de dados.

## Como fazer:

Para capitalizar uma string em PHP, você pode usar a função interna `ucfirst()`. Isso irá alterar o primeiro caractere da string para letra maiúscula.

```
<?php
$palavra = "programação";
echo ucfirst($palavra); // saída: Programação
?>
```

Para capitalizar todas as palavras em uma string, incluindo as palavras conectivas ("e", "de", "para", etc.), podemos usar a função `ucwords()`.

```
<?php
$frase = "aprendendo a programar em php";
echo ucwords($frase); // saída: Aprendendo A Programar Em Php
?>
```

## Mergulho Profundo:

Além das funções `ucfirst()` e `ucwords()`, existem outras opções para capitalizar strings em PHP. Uma delas é a função `mb_convert_case()`, que permite especificar se apenas o primeiro caractere, todas as palavras ou a string inteira serão capitalizadas.

Além disso, é importante estar ciente das diferenças de capitalização entre os idiomas. Por exemplo, em algumas línguas, a letra "i" maiúscula é escrita como "İ". Nesses casos, é recomendável usar a função `mb_convert_case()` ao invés de `ucfirst()` ou `ucwords()` para garantir a capitalização correta.

## Veja também:

- Documentação oficial do PHP sobre `ucfirst()`: https://www.php.net/manual/pt_BR/function.ucfirst.php
- Documentação oficial do PHP sobre `ucwords()`: https://www.php.net/manual/pt_BR/function.ucwords.php
- Documentação oficial do PHP sobre `mb_convert_case()`: https://www.php.net/manual/pt_BR/function.mb-convert-case.php