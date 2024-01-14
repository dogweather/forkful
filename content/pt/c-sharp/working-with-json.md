---
title:                "C#: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON

JSON (JavaScript Object Notation) é um formato de dados popular e amplamente utilizado na programação. Ele permite que os dados sejam armazenados e transmitidos de forma eficiente entre diferentes sistemas. Além disso, é fácil de ler e entender para humanos e máquinas.

## Como trabalhar com JSON em C#

Para trabalhar com JSON em C#, é necessário utilizar a biblioteca Newtonsoft.Json, que é uma das mais populares para manipular esse formato de dados. A seguir, um exemplo de como criar um objeto JSON e convertê-lo para uma string:

```
using Newtonsoft.Json;

...

// Criando um objeto JSON
var pessoa = new { nome = "João", idade = 30 };

// Convertendo o objeto para uma string JSON
string json = JsonConvert.SerializeObject(pessoa);

// Output: {"nome":"João","idade":30}
Console.WriteLine(json);
```

Para converter uma string JSON de volta para um objeto C#, podemos usar o método `JsonConvert.DeserializeObject`:

```
// Convertendo uma string JSON para um objeto C#
var pessoa = JsonConvert.DeserializeObject<object>(json);

// Acessando os dados do objeto
Console.WriteLine(pessoa.nome); // Output: João
Console.WriteLine(pessoa.idade); // Output: 30
```

## Aprofundando em JSON

Além de criar e converter objetos JSON, também é possível trabalhar com arquivos JSON. Por exemplo, podemos ler um arquivo JSON e desserializá-lo para um objeto C#:

```
// Lendo um arquivo JSON
string json = File.ReadAllText("pessoa.json");

// Desserializando a string JSON para um objeto C#
var pessoa = JsonConvert.DeserializeObject<object>(json);

// Acessando os dados do objeto
Console.WriteLine(pessoa.nome); // Output: Maria
Console.WriteLine(pessoa.idade); // Output: 25
```

Também é possível manipular e adicionar dados a um objeto JSON já existente usando a classe `JObject`:

```
// Lendo um arquivo JSON
string json = File.ReadAllText("pessoa.json");

// Criando um objeto JObject
JObject jObject = JObject.Parse(json);

// Adicionando uma nova propriedade
jObject.Add("profissao", "engenheira");

// Output: {"nome":"Maria","idade":25,"profissao":"engenheira"}
Console.WriteLine(jObject.ToString());
```

## Veja também

- Documentação oficial da biblioteca Newtonsoft.Json: https://www.newtonsoft.com/json
- Tutorial em vídeo de como trabalhar com JSON em C#: https://www.youtube.com/watch?v=2HswCnyP9Dk
- Artigo sobre como manipular objetos JSON com a classe `JObject`: https://www.juniordevelopercentral.com/trabalhando-com-json-jobject/