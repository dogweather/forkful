---
title:                "C#: Utilizando expressões regulares"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em programação?

Expressões regulares são uma ferramenta muito útil em programação para buscar e manipular sequências de caracteres de forma eficiente. Elas permitem que você encontre padrões específicos em uma string e substitua ou manipule esses padrões de acordo com suas necessidades. 

Muitas vezes, trabalhar com textos pode ser uma tarefa difícil e tediosa, mas com o uso de expressões regulares, essa tarefa se torna muito mais fácil e rápida. Portanto, elas são amplamente utilizadas em diversas linguagens de programação, incluindo C#.

## Como usar expressões regulares em C#

Para utilizar expressões regulares em C#, você precisará importar o namespace System.Text.RegularExpressions no início do seu código. Em seguida, pode utilizar as classes e métodos dessa biblioteca para criar e aplicar as suas expressões regulares.

Um exemplo simples seria a criação de uma expressão regular para validar um endereço de e-mail. Veja o trecho de código abaixo:

```C#
string email = "exemplo@email.com";
Regex regex = new Regex(@"^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}$"); 
// Expressão regular que valida um endereço de e-mail
if (regex.IsMatch(email)) 
{
    Console.WriteLine("Endereço de e-mail válido");
}
else 
{
    Console.WriteLine("Endereço de e-mail inválido");
}
```

Neste exemplo, utilizamos o método IsMatch da classe Regex para verificar se o endereço de e-mail fornecido atende ao padrão definido pela expressão regular. Se sim, a mensagem "Endereço de e-mail válido" é exibida, caso contrário, a mensagem "Endereço de e-mail inválido" é exibida.

É importante lembrar que expressões regulares são sensíveis a maiúsculas e minúsculas, portanto, deve-se ter atenção ao definir os padrões. Além disso, existem diversos caracteres especiais que podem ser utilizados em expressões regulares para torná-las mais precisas e flexíveis.

## Aprofundando nas expressões regulares

Existem muitos recursos e possibilidades ao utilizar expressões regulares em suas aplicações. É possível, por exemplo, extrair informações específicas de uma string, substituir padrões por outros textos, realizar validações mais complexas, dentre outras funcionalidades.

É importante estudar e praticar o uso de expressões regulares para se familiarizar com suas possibilidades e melhorar suas habilidades de programação. Além disso, existem diversos recursos online, como tutoriais e websites, que podem auxiliar no estudo e aprimoramento dessa ferramenta.

## Veja também

- [Documentação oficial do C# para expressões regulares](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Tutorial sobre expressões regulares em C#](https://www.devmedia.com.br/programacao/dotnet/expressoes-regulares-em-c-sharp/35505)
- [Testador de expressões regulares online](https://regex101.com/)