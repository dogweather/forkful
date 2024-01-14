---
title:                "PHP: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

##Por Que Usar Expressões Regulares no PHP?

Expressões regulares são uma ferramenta poderosa para manipular texto em qualquer linguagem de programação. No PHP, elas permitem buscar, substituir, validar e extrair informações de strings de maneira eficiente e flexível. Se você trabalha com processamento de dados, busca de informações ou validação de entradas de usuário, as expressões regulares são uma habilidade útil a se ter no seu kit de ferramentas de desenvolvimento.

## Como Usar Expressões Regulares no PHP

Para usar expressões regulares no PHP, primeiro é necessário delimitar o padrão de busca utilizando caracteres especiais. Depois, é preciso escolher uma função de expressão regular adequada à sua necessidade. Por exemplo, a função preg_match() irá retornar verdadeiro se o padrão for encontrado na string, enquanto a função preg_replace() irá substituir o padrão encontrado por outro valor. Veja alguns exemplos:

```PHP 
// Verifica se o e-mail inserido é válido
$email = "exemplo@exemplo.com";
if (preg_match("/^\w+@[a-zA-Z_]+?\.[a-zA-Z]{2,3}$/", $email)) {
  echo "E-mail válido!";
}
// Saída: E-mail válido!

// Remove todas as letras minúsculas de uma string
$frase = "Esta é uma frase com as palavras em minúsculas.";
$frase_upper = preg_replace("/[a-z]/", "", $frase);
echo $frase_upper;
// Saída: ESTA FRASE 

```

## Profundidade nas Expressões Regulares do PHP

As expressões regulares podem parecer complicadas à primeira vista, mas quanto mais você se aprofunda, mais facilidade terá em utilizá-las. Existem diversos recursos e regras que podem ser aplicados para tornar suas expressões ainda mais poderosas, como adicionar opções de busca, utilizar padrões recursivos ou mesmo criar grupos de captura para extrair informações específicas da string.

É importante também lembrar que as expressões regulares podem variar de acordo com a versão do PHP. Por isso, sempre consulte a documentação oficial para garantir que está utilizando a sintaxe correta.

## Veja Também

- [Documentação oficial do PHP sobre expressões regulares](https://www.php.net/manual/en/pcre.pattern.php)
- [Tutorial sobre expressões regulares no PHP](https://www.tutorialrepublic.com/php-tutorial/php-regular-expressions.php)
- [Ferramenta online para testar suas expressões regulares](https://regex101.com/)