---
aliases:
- /pt/java/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:16.540777-07:00
description: "Trabalhar com JSON (Nota\xE7\xE3o de Objeto JavaScript) significa manipular\
  \ esse formato leve de interc\xE2mbio de dados dentro de suas aplica\xE7\xF5es Java.\u2026"
lastmod: 2024-02-18 23:08:58.040111
model: gpt-4-0125-preview
summary: "Trabalhar com JSON (Nota\xE7\xE3o de Objeto JavaScript) significa manipular\
  \ esse formato leve de interc\xE2mbio de dados dentro de suas aplica\xE7\xF5es Java.\u2026"
title: Trabalhando com JSON
---

{{< edit_this_page >}}

## O Que & Por Que?
Trabalhar com JSON (Notação de Objeto JavaScript) significa manipular esse formato leve de intercâmbio de dados dentro de suas aplicações Java. Programadores optam pelo JSON para serializar e transmitir dados estruturados por uma rede e facilmente configurar e armazenar dados porque é legível por humanos e independente de linguagem.

## Como fazer:
Vamos arregaçar as mangas e começar a codificar com JSON em Java.

Primeira coisa, você precisará de uma biblioteca de processamento JSON como `Jackson` ou `Google Gson`. Aqui usaremos `Jackson`, então adicione essa dependência ao seu `pom.xml`:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Agora, vamos serializar (escrever) um simples objeto Java para JSON:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class ExemploJson {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Pessoa pessoa = new Pessoa("Alex", 30);
            String json = mapper.writeValueAsString(pessoa);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Pessoa {
    public String nome;
    public int idade;

    public Pessoa(String nome, int idade) {
        this.nome = nome;
        this.idade = idade;
    }
}
```

A saída deve ser:

```json
{"nome":"Alex","idade":30}
```

Agora, para desserializar (ler) JSON de volta para um objeto Java:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class ExemploJson {
    public static void main(String[] args) {
        String json = "{\"nome\":\"Alex\",\"idade\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Pessoa pessoa = mapper.readValue(json, Pessoa.class);
            System.out.println(pessoa.nome + " tem " + pessoa.idade + " anos.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

A saída será:

```
Alex tem 30 anos.
```

## Aprofundamento
A simplicidade e eficácia do JSON fizeram dele o padrão de facto para troca de dados na web, superando o XML. Introduzido no início dos anos 2000, JSON foi derivado do JavaScript, mas agora é suportado pela maioria das linguagens.

Alternativas ao JSON incluem XML, que é mais verboso, e formatos binários como Protocol Buffers ou MessagePack, que são menos legíveis por humanos, mas mais eficientes em tamanho e velocidade. Cada um tem seus casos de uso; a escolha depende das suas necessidades de dados específicas e contexto.

No Java, além do `Jackson` e `Gson`, temos também `JsonB` e `org.json` como outras bibliotecas para manipular JSON. Jackson oferece processamento baseado em fluxo e é conhecido pela velocidade, enquanto Gson é celebrado pela sua facilidade de uso. O JsonB faz parte do Jakarta EE, oferecendo uma abordagem mais padronizada.

Ao implementar JSON, lembre-se de tratar suas exceções adequadamente - seu código deve ser robusto contra entradas ruins. Além disso, considere as implicações de segurança do vinculação automática de dados – sempre valide suas entradas!

## Veja Também
- [Projeto Jackson](https://github.com/FasterXML/jackson)
- [Projeto Gson](https://github.com/google/gson)
- [Especificação JSON](https://www.json.org/json-pt.html)
- [Especificação JsonB](https://jakarta.ee/specifications/jsonb/)
