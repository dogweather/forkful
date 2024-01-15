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

## Por que

Se você é um desenvolvedor web ou apenas um curioso sobre como as coisas funcionam, é provável que em algum momento da sua jornada na internet você já tenha se perguntado como é possível baixar uma página web e vê-la em seu navegador. Neste artigo, vamos explorar como realizar essa tarefa usando PHP.

## Como Fazer

Baixar uma página web com PHP é um processo simples e direto. Primeiro, precisamos criar uma instância de uma classe chamada ```Curl```, que é responsável por fazer requisições HTTP. Em seguida, fornecemos a URL desejada e chamamos o método ```exec()```, que irá fazer a requisição e armazenar a resposta.

```
<?php
$curl = new Curl();
$response = $curl->exec('https://www.example.com');
echo $response;
?>
```

Ao executar este código, você deverá ver o conteúdo da página sendo impresso em seu navegador. No entanto, muitas vezes não queremos simplesmente imprimir a resposta, mas sim armazená-la para uso posterior. Para isso, podemos usar o método ```getBody()``` da classe ```Curl```, que irá retornar o conteúdo da página como uma string.

```
<?php
$curl = new Curl();
$response = $curl->exec('https://www.example.com');
$body = $curl->getBody();
echo $body;
?>
```

Agora que sabemos como fazer uma requisição e obter a resposta, também podemos enviar dados para o servidor. Vamos imaginar que queremos preencher um formulário e submetê-lo. Para isso, usamos o método ```setOpt()``` da classe ```Curl```, passando como parâmetro um array com os dados a serem enviados.

```
<?php
$curl = new Curl();
$data = array(
    'nome' => 'Maria',
    'sobrenome' => 'Silva',
    'email' => 'maria@example.com'
);
$curl->setOpt(CURLOPT_POSTFIELDS, $data);
$response = $curl->exec('https://www.example.com/processa_formulario.php');
echo $response;
?>
```

## Deep Dive

Para realmente entendermos como é possível baixar uma página web com PHP, precisamos olhar para a classe ```Curl``` em mais detalhes. Ela é um wrapper da biblioteca cURL, que é uma ferramenta poderosa para fazer requisições HTTP. A classe ```Curl``` possui diversos métodos que nos permitem controlar as configurações da requisição, como adicionar headers, definir timeouts e autenticação.

Além disso, a classe também possui métodos para realizar operações mais avançadas, como seguir redirecionamentos e fazer upload de arquivos. Se você quiser se aprofundar ainda mais no assunto, pode consultar a documentação oficial da classe ```Curl``` e da biblioteca cURL.

## Veja Também

- [Documentação oficial da classe Curl](https://www.php.net/manual/en/class.curl.php)
- [Documentação oficial da biblioteca cURL](https://curl.haxx.se/libcurl/)