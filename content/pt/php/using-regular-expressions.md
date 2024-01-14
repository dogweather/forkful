---
title:    "PHP: Utilizando expressões regulares"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que utilizar expressões regulares em programação PHP?
As expressões regulares são uma ferramenta poderosa para manipulação de texto em programas de PHP. Elas permitem que você procure por padrões específicos em uma string e faça alterações ou extrações com base neles. Se você trabalha com manipulação de strings em seu código, então as expressões regulares podem ser uma grande ajuda para tornar as coisas mais eficientes e eficazes.

## Como utilizar expressões regulares em PHP
Para começar, certifique-se de que seu ambiente PHP esteja configurado com suporte para expressões regulares. Depois disso, a sintaxe básica é a seguinte:

```
$padrao = "/regular expression/";
$string = "exemplo de texto com padrão de correspondência";
if (preg_match($padrao, $string)) {
  echo "Padrão encontrado!";
} else {
  echo "Padrão não encontrado.";
}
```

Neste exemplo, o padrão "/regular expression/" é procurado na string "exemplo de texto com padrão de correspondência". Se houver correspondência, a mensagem "Padrão encontrado!" será exibida.

Você também pode usar expressões regulares para substituir partes de uma string. Por exemplo:

```
$padrao = "/php/i";
$string = "Eu amo programar em PHP!";
$replaced = preg_replace($padrao, "JavaScript", $string);
echo $replaced;
```

Neste caso, o padrão "/php/i" é substituído por "JavaScript" na string, resultando na mensagem "Eu amo programar em JavaScript!".

## Um mergulho mais profundo em expressões regulares
As expressões regulares podem ser usadas para padrões mais complexos, como procurar por uma determinada quantidade de dígitos em um número de telefone ou extrair informações de um texto com formato específico. Existem muitos recursos disponíveis na documentação oficial do PHP para ajudá-lo a aprender mais sobre expressões regulares e como usá-las em diferentes cenários.

## Veja também
- [Documentação oficial das expressões regulares no PHP](https://www.php.net/manual/pt_BR/book.pcre.php)
- [Tutorial de expressões regulares em PHP (em inglês)](https://www.tutorialspoint.com/php/php_regular_expression.htm)
- [Exemplos práticos de utilização de expressões regulares em PHP (em português)](https://www.devmedia.com.br/introducao-ao-uso-de-expressoes-regulares-em-php/25337)

Com a devida prática e conhecimento, você poderá aproveitar ao máximo as expressões regulares em seus projetos de PHP. Elas podem economizar tempo e esforço na manipulação de strings e tornar seu código mais eficiente. Continue estudando e explorando o potencial das expressões regulares para dominar essa poderosa ferramenta em sua programação PHP.