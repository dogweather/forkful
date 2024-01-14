---
title:    "C#: Usando expressões regulares"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares?

As Expressões Regulares, também conhecidas como "regex", são uma ferramenta poderosa para manipulação de texto em linguagens de programação, incluindo C#. Com elas, é possível realizar buscas e substituições complexas em strings com apenas algumas linhas de código. Se você precisa validar dados de entrada, filtrar informações de um texto ou analisar padrões em uma string, as Expressões Regulares são a solução ideal.

## Como Usar Expressões Regulares em C#

Para começar, é necessário importar o namespace ```System.Text.RegularExpressions``` no seu código C#. Em seguida, você pode criar um objeto da classe Regex, passando como argumento a expressão regular que deseja utilizar. Por exemplo:

```C#
Regex regex = new Regex(@"\d{2}/\d{2}/\d{4}"); // Essa expressão verifica se há uma data no formato DD/MM/YYYY
```
Para realizar uma busca em uma string, utilize o método ```Match()``` do objeto Regex, passando como argumento a string que deseja verificar. O método retornará um objeto do tipo Match se encontrar um padrão correspondente ou ```null``` se não encontrar.

```C#
string texto = "Hoje é dia 28/04/2021.";
Match resultado = regex.Match(texto);

Console.WriteLine($"Data encontrada: {resultado.Value}"); // Saída: Data encontrada: 28/04/2021
```
Além disso, é possível utilizar grupos de captura para extrair informações específicas de uma string. Basta adicionar parênteses na sua expressão regular para definir um grupo e acessar o seu valor no objeto Match.

## Aprofundando em Expressões Regulares

As Expressões Regulares oferecem muitas possibilidades além da simples busca e substituição. Você pode utilizar metacaracteres como ```*```, ```+``` e ```?``` para definir quantificadores e tornar a sua expressão mais flexível. Também é possível utilizar classes de caracteres, como ```\w``` para encontrar letras e números, ou ```\s``` para encontrar espaços em branco.

Outro recurso interessante é a utilização de lookahead e lookbehind, que permitem verificar se uma determinada string está à frente ou atrás da posição atual de busca. Isso é muito útil para validar senhas, e-mails ou outras informações que precisam seguir um padrão específico.

## Veja Também

- [Documentação C# para Expressões Regulares](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Exemplos interativos de Expressões Regulares](https://regex101.com/)
- [Curso online gratuito de Expressões Regulares em C#](https://www.udemy.com/course/expressoes-regulares-em-c-desenvolvedor-microsoft/?referralCode=0C520A4F93C8A9651BFC)