---
title:                "Enviando uma solicitação http"
html_title:           "PHP: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que

Enviar uma solicitação HTTP é uma tarefa comum no desenvolvimento web. É uma maneira de obter dados de um servidor externo ou enviar informações para ele. Isso permite que os desenvolvedores criem aplicativos web mais dinâmicos e integrados.

## Como Fazer

Enviar uma solicitação HTTP em PHP é simples e pode ser feito em poucas etapas. Primeiro, é necessário criar uma instância da classe `CURL` para realizar a requisição. Em seguida, é preciso definir as configurações da solicitação e executá-la utilizando o método `execute()`. Por fim, o resultado da solicitação será retornado e poderá ser manipulado de acordo com as necessidades do desenvolvedor. Abaixo está um exemplo de código que realiza uma solicitação GET ao Google.com e exibe o código de resposta:

```
<?php
$ch = curl_init(); //cria uma instância da classe CURL
curl_setopt($ch, CURLOPT_URL, "https://www.google.com/"); //define a URL alvo
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true); //define a opção para retornar o resultado em vez de exibi-lo
$response = curl_exec($ch); //executa a requisição GET e retorna o resultado
echo $response; //exibe o resultado
curl_close($ch); //fecha a instância
```

O código acima irá imprimir o HTML da página inicial do Google. Este é apenas um exemplo básico e é possível configurar várias opções, como adicionar cabeçalhos à solicitação ou enviar dados através de POST ou PUT.

## Mergulho Profundo

Além das opções mencionadas na seção "Como Fazer", a classe CURL também possui outras configurações que podem ser úteis ao enviar solicitações HTTP. Por exemplo, é possível definir um tempo limite para a solicitação utilizando a opção `CURLOPT_TIMEOUT`. Isso pode evitar que o script fique esperando indefinidamente por uma resposta.

Existem também opções específicas para lidar com protocolos de segurança, como `CURLOPT_SSL_VERIFYHOST` e `CURLOPT_SSL_VERIFYPEER`, que permitem especificar se a conexão deve ser verificada no lado do servidor ou do cliente.

Além disso, a classe CURL também possui métodos como `curl_getinfo()` e `curl_error()`, que permitem obter informações sobre a solicitação e possíveis erros que possam ocorrer.

## Veja Também

- Documentação oficial do PHP sobre a classe CURL: https://www.php.net/manual/pt_BR/book.curl.php
- Tutorial em português sobre como realizar solicitações HTTP em PHP: https://www.devmedia.com.br/fazendo-requisicoes-http-com-o-php-curl/27296
- Livro "PHP for Absolute Beginners" (em inglês), que possui uma seção dedicada a enviar solicitações HTTP: https://www.amazon.com/dp/1484232451/