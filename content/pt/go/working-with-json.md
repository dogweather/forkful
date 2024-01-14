---
title:                "Go: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON em Go?

JSON (JavaScript Object Notation) é um formato popular para armazenar e transmitir dados em aplicações web. Em Go, trabalhar com JSON pode ser extremamente útil para ler e escrever dados estruturados, permitindo que diferentes sistemas se comuniquem de forma eficiente. Além disso, a sintaxe simples e legível do JSON o torna uma ótima opção para troca de dados entre diferentes linguagens de programação.

## Como trabalhar com JSON em Go

Para iniciar o trabalho com JSON em Go, é necessário importar o pacote "encoding/json". A primeira etapa é criar uma estrutura de dados em Go que represente a estrutura do JSON que deseja manipular. Em seguida, podemos utilizar a função "Marshal" do pacote encoding/json para converter a estrutura de dados em formato JSON.

```
package main

import (
    "encoding/json"
    "fmt"
)

type Pessoa struct {
    Nome    string
    Idade   int
    Cidade  string
}
    
func main() {
    // Criando uma estrutura de dados em Go
    pessoa := Pessoa{"João", 30, "São Paulo"}
    
    // Convertendo a estrutura para formato JSON
    jsonPessoa, _ := json.Marshal(pessoa)
    
    // Imprimindo o resultado
    fmt.Println(string(jsonPessoa))
}
```

A saída será:

```
{"Nome":"João","Idade":30,"Cidade":"São Paulo"}
```

Também podemos converter um JSON para uma estrutura de dados em Go utilizando a função "Unmarshal" do pacote encoding/json. Para isso, é necessário fornecer um ponteiro para a estrutura de dados que receberá os dados e o JSON a ser convertido.

```
func main() {
    // JSON de exemplo
    jsonPessoa := []byte(`{"Nome":"Maria","Idade":25,"Cidade":"Rio de Janeiro"}`)
    
    // Estrutura de dados em Go que receberá os dados do JSON
    var pessoa Pessoa
    
    // Convertendo o JSON para a estrutura de dados
    json.Unmarshal(jsonPessoa, &pessoa)
    
    // Imprimindo os dados da estrutura
    fmt.Println(pessoa.Nome)
    fmt.Println(pessoa.Idade)
    fmt.Println(pessoa.Cidade)
}
```

A saída será:

```
Maria
25
Rio de Janeiro
```

É importante ressaltar que para o processo de conversão ser bem-sucedido, os campos da estrutura de dados devem ter as mesmas keys (chaves) do JSON.

## Mergulho mais profundo no mundo do JSON em Go

Trabalhar com JSON em Go envolve mais do que apenas converter dados para diferentes formatos. Existe uma grande variedade de funções disponíveis no pacote encoding/json que permitem manipular dados JSON com mais precisão. Por exemplo, é possível fazer a leitura de arquivos JSON diretamente em uma estrutura de dados em Go, utilizando as funções "Unmarshal" ou "Decode" do pacote.

Além disso, é importante entender a diferença entre os tipos de dados utilizados em JSON e em Go. Por exemplo, strings em JSON sempre são representadas com "", enquanto em Go é possível utilizar tanto "" quanto ''.

Também é recomendado utilizar tags em nossas estruturas de dados em Go, para indicar como queremos que cada campo seja convertido para JSON. Isso é especialmente útil quando se trabalha com APIs, onde é comum receber respostas em JSON e precisamos mapear os dados para nossa estrutura de dados em Go.

## Veja também

- Documentação oficial do pacote encoding/json em Go: https://golang.org/pkg/encoding/json/
- Exemplos práticos de como trabalhar com JSON em Go: https://www.programming-books.io/essential/go/knowledge/json-in-go-748653d542e74b658fb9ba02db70beda
- Diferenças entre tipos de dados em JSON e Go: https://yourbasic.org/golang/json-example/
- Uso de tags em estruturas de dados em Go: https://tutorialedge.net/golang/go-structs-tutorial/