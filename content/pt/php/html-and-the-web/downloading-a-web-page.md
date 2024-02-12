---
title:                "Baixando uma página da web"
aliases:
- /pt/php/downloading-a-web-page.md
date:                  2024-01-20T17:44:35.029977-07:00
model:                 gpt-4-1106-preview
simple_title:         "Baixando uma página da web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Baixar uma página web significa puxar o seu conteúdo diretamente via Internet para processamento local. Programadores fazem isso para análise de dados, monitoramento de sites ou integração com outros serviços.

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
