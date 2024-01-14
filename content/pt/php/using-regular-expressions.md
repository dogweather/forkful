---
title:    "PHP: Usando expressões regulares"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares em Programação PHP?

As Expressões Regulares são uma ferramenta poderosa para manipular e validar dados em uma aplicação PHP. Elas permitem que você procure, substitua ou valide padrões específicos em strings, economizando tempo e esforço no processamento de dados.

## Como usar Expressões Regulares em PHP

Existem duas maneiras de usar Expressões Regulares em PHP: com a função preg_match() ou com o operador preg_replace(). Ambos os métodos requerem um padrão e uma string de entrada como parâmetros.

Exemplo de uso com preg_match():

```PHP
$string = "Olá, eu sou um programador PHP";

if (preg_match("/programador/", $string)) {
  echo "Encontrou a palavra programador";
} else {
  echo "Não encontrou a palavra programador";
}
```

Exemplo de saída:

```
Encontrou a palavra programador
```

Exemplo de uso com preg_replace():

```PHP
$string = "Hoje é um lindo dia para programar";

$novaString = preg_replace("/programar/", "codificar", $string);
echo $novaString;
```

Exemplo de saída:

```
Hoje é um lindo dia para codificar
```

## Mergulho Profundo em Expressões Regulares

Além das funções preg_match() e preg_replace(), existem muitos metacaracteres e quantificadores que podem ser usados em Expressões Regulares para buscar padrões mais complexos. Alguns exemplos são:

- . (ponto): representa qualquer caractere
- * (asterisco): corresponde a zero ou mais ocorrências do padrão anterior
- ^ (circunflexo): marca o início de uma string
- $ (cifrão): marca o final de uma string

É importante lembrar que os padrões são case-sensitive, a menos que a flag "i" seja usada após a expressão regular.

Para mais informações e exemplos, consulte a [documentação oficial do PHP sobre Expressões Regulares](https://www.php.net/manual/pt_BR/book.pcre.php).

## Veja Também

- [Tutorial sobre Expressões Regulares em PHP](https://www.devmedia.com.br/expressoes-regulares-no-php-aprenda-a-utilizar/33517)
- [Ferramenta Online para testar Expressões Regulares](https://regex101.com/)
- [Exemplos Reais de Uso de Expressões Regulares em PHP](https://www.geeksforgeeks.org/real-life-examples-use-regular-expressions-python/)