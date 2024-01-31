---
title:                "Capitalizando uma string"
date:                  2024-01-19
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Capitalizar uma string significa transformar todas as letras de um texto para maiúsculas. Programadores fazem isso para padronizar dados, melhorar a legibilidade ou atender a certas especificações técnicas.

## Como Fazer:
Capitalizar uma string em PHP é simples. Use a função `strtoupper()` para converter todas as letras para maiúsculas, ou `ucfirst()` para capitalizar apenas a primeira letra. Aqui estão alguns exemplos:

```PHP
<?php
// Capitalizando toda a string
$texto = "olá, mundo!";
$textoMaiusculo = strtoupper($texto);
echo $textoMaiusculo; // Saída: OLÁ, MUNDO!

// Capitalizando apenas a primeira letra
$textoPrimeiraMaiuscula = ucfirst($texto);
echo $textoPrimeiraMaiuscula; // Saída: Olá, mundo!
?>
```

## Mergulho Profundo
Historicamente, capitalizar texto tem suas raízes na era das máquinas de escrever e nos primeiros dias da computação, onde diferenciar informações importantes era feito com letras maiúsculas. Em PHP, a função `strtoupper()` tem sido utilizada desde as primeiras versões do idioma para converter uma string para todas maiúsculas.

Existem alternativas para diferentes casos de uso, como `ucwords()` para capitalizar a primeira letra de cada palavra em uma string, útil para nomes próprios ou títulos. A função `mb_strtoupper()` é uma alternativa que suporta múltiplos encodings de caracteres, sendo vital para strings com caracteres multibyte como em idiomas asiáticos ou acentuações.

Quanto à implementação, o PHP internamente faz uso de tabelas de caracteres e encodings específicos para transformar cada letra minúscula em sua equivalente maiúscula, levando em consideração o locale (configuração regional) quando necessário.

## Veja Também

- A documentação oficial das funções de manipulação de strings no PHP: https://www.php.net/manual/pt_BR/ref.strings.php
- Sobre encodings e o PHP: https://www.php.net/manual/pt_BR/mbstring.php
- Guia de estilo e práticas recomendadas para PHP: https://www.php-fig.org/psr/
