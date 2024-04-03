---
date: 2024-01-20 17:44:35.029977-07:00
description: "Como Fazer: Aqui est\xE1 um exemplo b\xE1sico usando cURL em PHP."
lastmod: '2024-03-13T22:44:46.666149-06:00'
model: gpt-4-1106-preview
summary: "Aqui est\xE1 um exemplo b\xE1sico usando cURL em PHP."
title: "Baixando uma p\xE1gina da web"
weight: 42
---

## Como Fazer:
Aqui está um exemplo básico usando cURL em PHP:

```php
<?php
$ch = curl_init("http://exemplo.com");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$conteudo = curl_exec($ch);
curl_close($ch);

echo $conteudo;
?>
```

Se bem-sucedido, você verá o HTML da página http://exemplo.com.

## Mergulho Profundo:
Historicamente, a função `file_get_contents()` era comum para baixar páginas, mas cURL oferece mais flexibilidade e opções. Alternativamente, você pode usar bibliotecas como Guzzle para uma abordagem mais abstrata e recursos. No exemplo acima, o `curl_setopt()` com `CURLOPT_RETURNTRANSFER` informa ao cURL para devolver a resposta ao invés de imprimi-la.

## Veja Também:
- [Documentação PHP cURL](https://www.php.net/manual/pt_BR/book.curl.php)
- [GuzzleHTTP - Cliente HTTP PHP](http://docs.guzzlephp.org/en/stable/)
- [Tutorial W3Schools sobre PHP file_get_contents()](https://www.w3schools.com/php/func_filesystem_file_get_contents.asp)
