---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Baixando uma Página Web com Swift: Guia passo a passo

## Por quê e para quê?

Baixar uma página da web significa adquirir seu conteúdo codificado em HTML. Os programadores fazem isso para analisar, extrair dados, verificar alterações ou fazer uma versão offline.

## Como:

Swift torna isso fácil. Podemos usar o Data para baixar o conteúdo HTML:

```Swift
let url = URL(string: "https://www.example.com")!
let htmlData = try! Data(contentsOf: url)
let htmlString = String(data: htmlData, encoding: .utf8)!
print(htmlString)
```

E para baixar uma página da internet de forma assíncrona, preferencialmente usando URLSession:

```Swift
let url = URL(string: "https://www.example.com")!
let task = URLSession.shared.dataTask(with: url) {(data, response, error) in
    if let data = data {
        let htmlString = String(data: data, encoding: .utf8)!
        print(htmlString)
    }
}
task.resume()
```

## Mergulho Profundo

Swift não é a única linguagem que você pode usar para isso, nem foi a primeira. Perl, por exemplo, foi amplamente utilizada para web scraping antes do Swift. Swift oferece vantagens, como melhor desempenho em comparação com Perl.

Usando bibliotecas externas, como AlamoFire ou SwiftyHTMLParser, você pode melhorar e simplificar suas operações de download do HTML.

Lembre-se, baixar páginas da web deve ser feito de maneira responsável e sempre respeitando os termos de serviço do site.

## Veja também

1. Documentação URLSession: https://developer.apple.com/documentation/foundation/urlsession
2. Guia da Stanford sobre Swift: http://web.stanford.edu/class/cs193p/
3. AlamoFire no GitHub: https://github.com/Alamofire/Alamofire
4. SwiftyHTMLParser no GitHub: https://github.com/touren/SwiftyHTMLParser