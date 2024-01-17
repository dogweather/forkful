---
title:                "Baixando uma página da web"
html_title:           "Swift: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que e porquê?
Fazer o download de uma página web significa baixar o conteúdo de uma URL para que possa ser visualizado em um dispositivo. Os programadores fazem isso para acessar informações necessárias para suas aplicações, ou para criar scrapers que coletam dados de várias páginas web.

## Como fazer:
```
let url = URL(string: "https://www.example.com") //criando um objeto URL
let session = URLSession.shared //criando uma sessão compartilhada
let task = session.dataTask(with: url!, completionHandler: {data, response, error in //configurando a tarefa para obter o conteúdo da URL
  if let content = data { //verificando se há dados baixados
    print(String(data: content, encoding: .utf8)) //convertendo os dados em string e imprimindo o conteúdo na codificação UTF-8
  }
})

task.resume() //iniciando a tarefa
```
### Saída:
```
<!doctype html>
<html>
<head>
  <title>Exemplo de página</title>
  <meta charset="UTF-8">
  <meta name="description" content="Esta é uma página de exemplo">
</head>
<body>
  <h1>Olá, mundo!</h1>
  <p>Esta é uma página de exemplo que demonstra o uso de Swift para baixar e mostrar o conteúdo de uma página web.</p>
</body>
</html>
```

## Aprofundando:
1. Contexto histórico: Antes do surgimento de Swift, a linguagem Objective-C era mais comumente usada para fazer o download de páginas web em dispositivos iOS/macOS.
2. Alternativas: Além do método mostrado acima usando `URLSession`, também é possível usar a biblioteca de terceiros Alamofire para fazer o download de páginas web em Swift.
3. Detalhes de implementação: A tarefa `dataTask` é executada de forma assíncrona, o que significa que o código continuará a ser executado enquanto o download está em andamento. O bloco `completionHandler` é chamado depois que o download for concluído, e nele podemos acessar os dados baixados, a resposta do servidor e possíveis erros.

## Veja também:
- Documentação da Apple para URLSession: https://developer.apple.com/documentation/foundation/urlsession
- Documentação da biblioteca Alamofire: https://github.com/Alamofire/Alamofire