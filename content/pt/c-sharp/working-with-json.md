---
title:                "Trabalhando com json"
html_title:           "C#: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## O que e Porquê?
JSON (JavaScript Object Notation) é um formato leve e popular para troca de dados entre diferentes sistemas. Programadores frequentemente utilizam JSON para transmitir informações estruturadas entre um cliente e um servidor, ou para armazenar dados em um arquivo legível por máquina.

## Como fazer:
Para trabalhar com JSON em C#, é necessário importar a biblioteca "Newtonsoft.Json" e utilizar suas classes e métodos para serializar e deserializar objetos JSON. Por exemplo:

```
using Newtonsoft.Json; // importar biblioteca
...
string jsonString = "{\"nome\":\"João\", \"idade\":25}"; // objeto JSON
var pessoa = JsonConvert.DeserializeObject<Pessoa>(jsonString); // deserializando para objeto C#
Console.WriteLine(pessoa.nome); // imprimir resultado
```
Saída:
```
João
```

## Mergulho Profundo:
JSON foi criado originalmente por Douglas Crockford em 2001 como uma alternativa ao formato XML. Apesar de ser baseado em JavaScript, pode ser utilizado com várias linguagens de programação. Além da biblioteca "Newtonsoft.Json", existem outras opções para trabalhar com JSON em C#, como DataContractJsonSerializer e JavaScriptSerializer. É importante lembrar de validar e manipular corretamente os dados JSON recebidos, para evitar vulnerabilidades de segurança.

## Veja também:
- [Documentação oficial do Newtonsoft.Json](https://www.newtonsoft.com/json)
- [Tutorial do Microsoft sobre trabalhar com JSON em C#](https://docs.microsoft.com/en-us/dotnet/standard/serialization/system-text-json-how-to)
- [Comparação entre JSON e XML](https://www.w3schools.com/js/js_json_xml.asp)