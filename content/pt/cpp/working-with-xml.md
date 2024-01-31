---
title:                "Trabalhando com XML"
date:                  2024-01-26T04:28:34.236511-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/working-with-xml.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Trabalhar com XML significa analisar, criar e manipular dados XML (eXtensible Markup Language). Programadores gerenciam o XML para lidar com a troca de dados estruturados, configuração e mais, devido à sua natureza neutra de plataforma.

## Como:
Aqui está uma maneira simples de analisar XML usando a biblioteca TinyXML-2:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Olá, Mundo!</message></root>");
    const char* conteudo = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << conteudo << std::endl;
    return 0;
}
```

Saída de exemplo:

```
Olá, Mundo!
```

E assim se cria um arquivo XML:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* declaracao = doc.NewDeclaration();
    doc.InsertFirstChild(declaracao);
    auto* raiz = doc.NewElement("root");
    doc.InsertEndChild(raiz);
    auto* mensagem = doc.NewElement("message");
    mensagem->SetText("Olá, Mundo!");
    raiz->InsertEndChild(mensagem);
    doc.SaveFile("saida.xml");
    return 0;
}
```

Isso gera um arquivo XML `saida.xml` com o conteúdo:

```xml
<?xml version="1.0"?>
<root>
    <message>Olá, Mundo!</message>
</root>
```

## Aprofundamento
O XML tem sido fundamental em serviços web e armazenamento de dados desde o final dos anos 90. Enquanto JSON e YAML agora são mais comuns para configuração e interop, o XML ainda é enorme em muitos sistemas empresariais. Analisar XML em C++ pode parecer algo antigo com a análise manual de DOM/SAX. Felizmente, bibliotecas como TinyXML-2 simplificam isso. C++ não tem suporte embutido para XML; bibliotecas como TinyXML-2, pugixml, ou Xerces resolvem as partes difíceis.

## Veja Também
- Documentação do TinyXML-2: https://leethomason.github.io/tinyxml2/
- Biblioteca pugixml: https://pugixml.org/
- Analisador Xerces-C++: https://xerces.apache.org/xerces-c/
- Especificação XML da W3C: https://www.w3.org/XML/
