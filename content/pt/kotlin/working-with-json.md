---
title:                "Kotlin: Trabalhando com json."
simple_title:         "Trabalhando com json."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON?

O JSON (JavaScript Object Notation) é uma forma leve e fácil de representar e trocar dados entre diferentes aplicações. Se você está construindo uma aplicação web, mobile ou desktop, trabalhar com JSON pode ser uma tarefa importante para garantir que os dados sejam representados de forma eficiente e legível.

## Como trabalhar com JSON em Kotlin

Para trabalhar com JSON em Kotlin, primeiro é necessário importar a biblioteca Gson. Em seguida, é preciso definir a classe que representa os seus dados e mapeá-los com as devidas anotações. Por fim, é possível realizar a conversão de objetos para JSON e vice-versa utilizando os métodos da biblioteca Gson.

```Kotlin
// Importar a biblioteca Gson
import com.google.gson.Gson

// Definir a classe que representa os dados
data class Pessoa (val nome: String, val idade: Int, val profissão: String)

// Mapear a classe com as anotações necessárias
class MainActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        // Converter objeto para JSON
        val pessoa = Pessoa("Maria", 23, "Engenheira")
        val gson = Gson()
        val json = gson.toJson(pessoa)
        Log.d("JSON", json)

        // Converter JSON para objeto
        val json = "{\"nome\":\"João\",\"idade\":30,\"profissão\":\"Médico\"}"
        val gson = Gson()
        val pessoa = gson.fromJson(json, Pessoa::class.java)
        Log.d("Nome", pessoa.nome)
    }
}
```

O output do código será o seguinte:

```JSON
{"nome": "Maria", "idade": 23, "profissão": "Engenheira"}
```

## Aprofundando-se em JSON

Existem várias maneiras de trabalhar com JSON em Kotlin, além da biblioteca Gson, como por exemplo, a biblioteca Moshi. Também é possível manipular JSON diretamente utilizando a classe `JSONObject` da API do Android.

Além disso, é importante conhecer os diferentes tipos de dados suportados pelo JSON e como realizar a validação dos dados recebidos. É possível utilizar bibliotecas externas para realizar essa validação, como por exemplo, a biblioteca JSON-schema-validator.

## Veja também

- [Documentação oficial do Gson](https://github.com/google/gson/blob/master/UserGuide.md)
- [Documentação oficial do Moshi](https://github.com/square/moshi)
- [Documentação oficial da API do Android para JSONObject](https://developer.android.com/reference/org/json/JSONObject)
- [Biblioteca JSON-schema-validator](https://github.com/java-json-tools/json-schema-validator)