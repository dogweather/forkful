---
date: 2024-01-20 18:04:04.507321-07:00
description: "Iniciar um novo projeto de programa\xE7\xE3o \xE9 como colocar uma semente\
  \ na terra esperando que cres\xE7a e se transforme em uma \xE1rvore robusta. Programadores\u2026"
lastmod: '2024-03-13T22:44:46.668010-06:00'
model: gpt-4-1106-preview
summary: "Iniciar um novo projeto de programa\xE7\xE3o \xE9 como colocar uma semente\
  \ na terra esperando que cres\xE7a e se transforme em uma \xE1rvore robusta. Programadores\u2026"
title: Iniciando um novo projeto
---

{{< edit_this_page >}}

## O Que & Por Quê?
Iniciar um novo projeto de programação é como colocar uma semente na terra esperando que cresça e se transforme em uma árvore robusta. Programadores embarcam nessa jornada para transformar ideias em realidade, resolver problemas, aprender e também, por que não, para se divertirem um pouco.

## Como Fazer:
Começar um projeto PHP é simples. Você precisa de um ambiente de desenvolvimento básico. Instale um servidor local como XAMP, WAMP ou MAMP e um editor de texto, como VSCode ou PHPStorm. Depois, crie um arquivo `index.php`:

```PHP
<?php
echo "Olá, mundo!";
?>
```

No navegador, a saída será algo assim:

```
Olá, mundo!
```

## Mergulho Profundo
O PHP, uma linguagem de script do lado do servidor, chegou ao mundo em 1994 e desde então tem evoluído constantemente. Para início de projetos, houve tempos em que o PEAR e o PECL eram os caras na área. Hoje, usamos o Composer, um gerenciador de dependências que simplifica a adição de bibliotecas e frameworks ao nosso projeto.

Implementar um projeto do zero envolve escolhas. Frameworks como Laravel e Symfony podem turboar seu desenvolvimento com uma rica oferta de funcionalidades embutidas. Se preferir, você pode começar sem frameworks, o que é ótimo para aprender as tripas do PHP.

Aqui vai um exemplo simples para criar um projeto com Composer e incluir o framework Slim:

1. Instale o Composer globalmente.
2. Execute `composer require slim/slim:"4.*"` na raiz da pasta do seu projeto.
3. Crie um arquivo `index.php` utilizando Slim:

```PHP
<?php
use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\Factory\AppFactory;

require __DIR__ . '/vendor/autoload.php';

$app = AppFactory::create();

$app->get('/hello/{name}', function (Request $request, Response $response, array $args) {
    $name = $args['name'];
    $response->getBody()->write("Olá, $name");
    return $response;
});

$app->run();
```
Rode o servidor local e acesse `localhost/hello/mundo`. Você verá:

```
Olá, mundo
```

## Veja Também
Para expandir seu conhecimento e habilidades em PHP e gerenciamento de projetos, aqui estão alguns recursos:

- Documentação Oficial do PHP: [php.net/manual/pt_BR/](https://www.php.net/manual/pt_BR/)
- Composer: [getcomposer.org](https://getcomposer.org/)
- Laravel: [laravel.com](https://laravel.com/)
- Symfony: [symfony.com](https://symfony.com/)
- Slim Framework: [www.slimframework.com](https://www.slimframework.com/)
- PHP The Right Way (Guia da Boas Práticas PHP): [phptherightway.com](https://phptherightway.com/)
