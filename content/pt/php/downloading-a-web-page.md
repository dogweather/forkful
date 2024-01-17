---
title:                "Baixando uma página da web"
html_title:           "PHP: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que é e por que fazer download de uma página web?

Fazer o download de uma página web é basicamente obter o código HTML e os recursos (como imagens, vídeos ou arquivos CSS) de um determinado site. Isso geralmente é feito por programadores que precisam acessar essas informações para fins de desenvolvimento ou análise.

## Como fazer:

Para fazer o download de uma página web em PHP, você pode usar a função `file_get_contents()` para obter o conteúdo da página especificada. Por exemplo:

```
$página = file_get_contents('http://www.exemplo.com');
print $página; // Imprime o conteúdo da página
```

Você também pode utilizar a biblioteca cURL para fazer o download de uma página e definir opções de conexão mais avançadas, como cookies, autenticação e cabeçalhos personalizados. Exemplo:

```
// Inicializa uma sessão cURL
$curl = curl_init();

// Define a URL alvo e outras opções
curl_setopt($curl, CURLOPT_URL, 'http://www.exemplo.com');
curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);

// Executa a requisição e obtém o conteúdo da página
$página = curl_exec($curl);

// Encerra a sessão cURL
curl_close($curl);

print $página; // Imprime o conteúdo da página
```

## Aprofundando:

Fazer o download de páginas web é uma tarefa comum na programação, seja para obter informações de um site externo ou para automatizar tarefas. Antes do PHP 5, essa tarefa era realizada principalmente com a função `fopen()` e suas variantes, mas agora é mais comum utilizar as funções `file_get_contents()` ou cURL por serem mais simples e flexíveis.

Além disso, existem também bibliotecas dedicadas ao download de páginas e análise de conteúdo, como o cURL, o Guzzle e o Simple HTML DOM.

## Veja também:

- Documentação oficial do PHP sobre [file_get_contents()](https://www.php.net/manual/pt_BR/function.file-get-contents.php) e [cURL](https://www.php.net/manual/pt_BR/book.curl.php)
- Artigo do blog da Locaweb sobre [download de páginas web em PHP](https://blog.locaweb.com.br/dica-de-script-como-realizar-download-de-paginas-em-php/)