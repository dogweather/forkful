---
title:                "C#: Analisando html."
simple_title:         "Analisando html."
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Por que 

Você já se perguntou como os navegadores são capazes de exibir páginas web com layouts complexos e elementos interativos? A resposta está na capacidade de interpretar e analisar o código HTML. Em outras palavras, o processo de "parsing" em HTML permite que os computadores entendam a estrutura e o conteúdo de uma página da web.

## Como Fazer

Para realizar o "parsing" de HTML em um programa C#, podemos usar a biblioteca HtmlAgilityPack. Esta biblioteca permite que desenvolvedores criem e manipulem o código HTML de forma eficiente e simplificada.

Primeiro, precisamos adicionar o pacote NuGet do HtmlAgilityPack ao nosso projeto. Isso pode ser feito facilmente através do Visual Studio ou usando o comando `Install-Package HtmlAgilityPack` no console do NuGet.

Uma vez que o pacote esteja instalado, podemos começar a "parsear" o HTML. Abaixo está um exemplo simples de como podemos buscar a tag `<title>` em um documento HTML e mostrar seu conteúdo:

```C#
var html = @"<html><head><title>Exemplo</title></head><body><h1>Bem-vindo</h1></body></html>";
var doc = new HtmlDocument();
doc.LoadHtml(html);

var title = doc.DocumentNode.SelectSingleNode("//title").InnerText;
Console.WriteLine(title); // Output: Exemplo
```

Neste exemplo, criamos uma string contendo um documento HTML e usamos o método `LoadHtml()` da biblioteca HtmlAgilityPack para carregar o HTML em um objeto `HtmlDocument`. Em seguida, usamos o método `SelectSingleNode()` para buscar o elemento `<title>` e obter seu conteúdo.

Mas e se quisermos buscar múltiplos elementos em uma página? Podemos fazer isso usando o método `SelectNodes()` e uma expressão XPath para especificar quais elementos queremos buscar. Por exemplo:

```C#
var html = @"<html><head><title>Exemplo</title></head><body><h1>Bem-vindo</h1></body></html>";
var doc = new HtmlDocument();
doc.LoadHtml(html);

var headings = doc.DocumentNode.SelectNodes("//h1");
foreach (var heading in headings)
{
    Console.WriteLine(heading.InnerText); // Output: Bem-vindo
}
```

Além de buscar elementos específicos em uma página, também podemos usar a biblioteca HtmlAgilityPack para navegar pela estrutura do HTML e fazer alterações no código, se desejado. Para mais detalhes sobre como usar essa biblioteca, consulte a documentação oficial.

## Mergulho Profundo

Ao trabalhar com HTML em C#, é importante entender a estrutura básica dos documentos HTML e como os elementos são organizados dentro do DOM (modelos de objeto de documento). Elementos HTML podem ter atributos, valores e conteúdo que podem ser extraídos usando as técnicas mencionadas acima. Além disso, diferentes tipos de elementos também podem ser encontrados em diferentes partes do documento, por isso é importante ter um bom conhecimento dos seletores XPath.

Também é importante lembrar que nem todas as páginas são escritas com HTML válido, o que pode tornar o processo de fazer "parse" em alguns documentos mais desafiador. Felizmente, a biblioteca HtmlAgilityPack é muito robusta em relação a isso e pode manipular HTML inválido de forma eficiente.

## Veja Também

- [Documentação oficial do HtmlAgilityPack](https://html-agility-pack.net/)
- [Guia de XPATH para HTML](https://www.w3schools.com/xml/xpath_syntax.asp)
- [Tutorial para iniciantes sobre HTML](https://www.w3schools.com/html/)