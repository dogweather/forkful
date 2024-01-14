---
title:                "PHP: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Por que enviar uma solicitação HTTP com autenticação básica?

Enviar solicitações HTTP com autenticação básica é uma forma comum de acessar recursos protegidos em servidores web. Isso é especialmente útil em casos em que é necessário limitar o acesso a determinadas informações ou funcionalidades apenas a usuários autenticados.

# Como fazer:

```PHP
<?php
// URL do recurso protegido
$url = "https://www.example.com/protegido.php";

// Credenciais de autenticação
$username = "usuario";
$password = "senha";

// Criando uma instância de cURL
$ch = curl_init();

// Definindo as opções para a solicitação HTTP
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");

// Enviando a solicitação e armazenando a resposta
$response = curl_exec($ch);

// Verificando se houve algum erro
if(curl_errno($ch)){
    echo 'Erro:' . curl_error($ch);
}

// Imprimindo a resposta
echo $response;

// Fechando a instância de cURL
curl_close($ch);
?>
```

**Saída:**

Ao enviar a solicitação com autenticação básica, o servidor irá verificar as credenciais e, se corretas, retornará o conteúdo desejado. Caso as credenciais sejam inválidas, o servidor retornará um erro 401 (Não Autorizado).

# Mergulho Profundo:

Ao enviar uma solicitação HTTP com autenticação básica, o cliente envia um cabeçalho "Authorization" junto com a solicitação. Esse cabeçalho contém as credenciais do usuário codificadas em base64 e o prefixo "Basic", indicando o tipo de autenticação utilizada.

Ao receber a solicitação, o servidor decodifica as credenciais e verifica sua validade. Se as credenciais estiverem corretas, o servidor retornará o conteúdo solicitado. Caso contrário, retornará um erro 401.

Vale ressaltar que a autenticação básica não é a forma mais segura de proteger um recurso, pois as credenciais são enviadas em texto claro no cabeçalho da requisição. Por isso, é recomendável usar um método mais seguro, como HTTPS, ao enviar informações confidenciais.

# Veja também:

- Documentação do cURL: https://www.php.net/manual/en/book.curl.php
- Como enviar solicitações HTTP com autenticação básica no Python: https://www.example.com/enviando-solicitacoes-http-com-autenticacao-basica-no-python