---
title:                "PHP: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que baixar uma página da web?

Baixar uma página da web é um processo muito útil para desenvolvedores e programadores de PHP. Isso permite que eles acessem o código fonte de uma página e usem as informações para diferentes propósitos, como análise de dados ou criação de ferramentas de automação.

## Como fazer

Aqui está um exemplo simples de como baixar uma página da web usando PHP:

```PHP
// Definir a URL da página a ser baixada
$url = "https://www.meusite.com/pagina";

// Iniciar a requisição do Curl
$ch = curl_init();

// Configurar as opções da requisição
curl_setopt($ch, CURLOPT_URL, $url); // URL da página
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true); // Retornar o resultado ao invés de imprimir
curl_setopt($ch, CURLOPT_HEADER, false); // Não incluir o cabeçalho no resultado

// Executar a requisição e armazenar o resultado em uma variável
$resultado = curl_exec($ch);

// Fechar a requisição
curl_close($ch);

// Imprimir o resultado
echo $resultado;
```

O código acima usa a biblioteca CURL, que é uma extensão do PHP usada para transferir dados entre servidores. A função `curl_init()` inicia a requisição e `curl_exec()` executa a mesma. É importante lembrar que a página deve estar acessível publicamente para que a função funcione corretamente.

Outra opção seria usar a função `file_get_contents()`, que pode fazer a mesma tarefa sem a necessidade de bibliotecas adicionais. No entanto, essa função é mais simples e não oferece tantas opções de configuração como a CURL.

## Deep Dive

Se você quer entender melhor como o processo de download de páginas da web funciona, saiba que ele é feito usando o protocolo HTTP. Esse protocolo define como os dados são transferidos entre servidores e clientes, permitindo que o PHP receba o conteúdo da página.

Ao fazer a requisição usando a função CURL ou `file_get_contents()`, o PHP envia uma solicitação para o servidor da página desejada e ele responde com os dados da página. Em seguida, o PHP armazena esses dados em uma variável, que pode ser usada para qualquer propósito necessário.

## Veja também

- [CURL documentation](https://www.php.net/manual/en/book.curl.php)
- [HTTP protocol overview](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
- [file_get_contents() documentation](https://www.php.net/manual/en/function.file-get-contents.php)