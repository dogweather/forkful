---
title:                "PHP: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Por que converter uma string para letras minúsculas?

Existem várias razões pelas quais alguém pode precisar converter uma string para letras minúsculas ao lidar com programação em PHP. Alguns motivos comuns incluem padronização de dados, facilitar a comparação de strings e evitar erros de case sensitivity.

# Como fazer isso em PHP?

Existem várias maneiras de converter uma string para letras minúsculas em PHP. Aqui estão alguns exemplos usando diferentes funções:

```PHP
// Usando a função strtolower()
$string = "ESTA É UMA STRING EM MAIÚSCULAS";
echo strtolower($string); // saída: "esta é uma string em maiúsculas"

// Usando a função mb_strtolower(), se sua string contém caracteres multibyte
$string = "ESSA É UMA STRING EM UTF-8";
echo mb_strtolower($string); // saída: "essa é uma string em utf-8"

// Usando a função strcasecmp() para comparar strings sem levar em conta o case
$str1 = "olá";
$str2 = "OLÁ";
echo strcasecmp($str1, $str2); // saída: 0 (strings são iguais)
```

# Informações adicionais sobre a conversão de strings para letras minúsculas

É importante notar que, ao converter uma string para letras minúsculas, a função irá considerar o charset do seu arquivo PHP. Se você não especificar o charset, a função usará o padrão definido nas configurações do PHP, que pode variar dependendo do servidor.

Além disso, é importante ter em mente que a conversão para letras minúsculas pode ter algumas variações em diferentes idiomas. Por exemplo, em algumas línguas, a letra "I" maiúscula pode ser convertida para "ı" minúscula, enquanto em outras, pode ser convertida para "i" minúscula. Então, é sempre importante testar e verificar se a conversão está sendo feita corretamente para o idioma em questão.

# Veja também

- [Documentação oficial do PHP para strtolower()](https://www.php.net/manual/pt_BR/function.strtolower.php)
- [Tutorial sobre case sensitivity e como lidar com isso em PHP](https://www.tutorialrepublic.com/php-tutorial/php-string-case-insensitive.php)
- [Informações sobre configurações de charset no PHP](https://www.php.net/manual/pt_BR/function.mb-internal-encoding.php)