---
date: 2024-01-26 03:40:38.034299-07:00
description: "Remover aspas de uma string em PHP significa eliminar aqueles caracteres\
  \ inc\xF4modos de aspas duplas (`\"`) ou simples (`'`) que podem atrapalhar a l\xF3\
  gica do\u2026"
lastmod: '2024-03-13T22:44:46.655584-06:00'
model: gpt-4-0125-preview
summary: "Remover aspas de uma string em PHP significa eliminar aqueles caracteres\
  \ inc\xF4modos de aspas duplas (`\"`) ou simples (`'`) que podem atrapalhar a l\xF3\
  gica do\u2026"
title: Removendo aspas de uma string
---

{{< edit_this_page >}}

## O Que & Por Quê?
Remover aspas de uma string em PHP significa eliminar aqueles caracteres incômodos de aspas duplas (`"`) ou simples (`'`) que podem atrapalhar a lógica do seu código ou consultas ao banco de dados. Os programadores fazem isso para limpar ou higienizar dados de entrada, garantindo que as strings sejam usadas ou armazenadas com segurança.

## Como fazer:
Aqui está um exemplo simples usando funções integradas do PHP:

```php
$quotedString = "'Olá,' ela disse, \"É um dia bonito!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // Saída: Olá, ela disse, É um dia bonito!
```

Simples, certo? Esta função `str_replace()` recebe um array de caracteres para remover da string, incluindo tanto aspas simples quanto duplas.

## Mergulho Profundo
Nos primeiros dias do PHP, os desenvolvedores tinham que ser extremamente cautelosos com as aspas em strings, especialmente ao inserir dados em um banco de dados. Aspas mal manipuladas poderiam levar a ataques de injeção SQL. Aí entram as magic quotes, um recurso que escapava automaticamente dados de entrada. Tornou-se obsoleto e foi finalmente removido porque incentivava práticas de codificação ruins e questões de segurança.

Agora, usamos funções como `str_replace()` ou regex com `preg_replace()` para padrões mais avançados. Aqui está um exemplo com regex:

```php
$quotedString = "'Olá,' ela disse, \"É um dia bonito!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

Para dados JSON, você pode usar `json_encode()` com opções como `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE` para evitar barras invertidas extras nas suas aspas.

Ao implementar, considere casos extremos. E se a sua string for destinada a ter certas aspas, como diálogos em uma história ou polegadas em medições? O contexto importa, então adapte a remoção de aspas ao uso pretendido dos dados.

## Veja Também
- [PHP: str_replace](https://www.php.net/manual/pt_BR/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/pt_BR/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/pt_BR/function.json-encode.php)
- [OWASP: Prevenção de Injeção SQL](https://owasp.org/www-community/attacks/SQL_Injection)
