---
title:                "C++: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

# Por que trabalhar com YAML?

Se você é um desenvolvedor de software, provavelmente já ouviu falar em YAML. Mas você sabe por que é importante trabalhar com esse formato de dados? YAML é uma linguagem de marcação moderna e flexível que permite a criação de documentos bem estruturados e legíveis tanto para humanos quanto para máquinas. Além disso, é amplamente utilizado em configurações de aplicativos, tornando-o essencial para qualquer programador.

## Como fazer

Para começar a trabalhar com YAML em seu código C++, você precisará de uma biblioteca ou framework que forneça suporte para esse formato. Uma opção popular é o "yaml-cpp", que pode ser instalado através de gerenciadores de pacotes como o "vcpkg".

Uma vez que sua biblioteca esteja instalada, você pode começar a usar YAML em seu código. Um exemplo simples seria criar um arquivo YAML com informações sobre um produto e, em seguida, carregá-lo em seu programa C++ para acessar esses dados. Veja um exemplo de código abaixo:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>

int main() {
    // Criando o arquivo YAML
    YAML::Emitter yaml;
    yaml << YAML::BeginMap;
    yaml << YAML::Key << "produto" << YAML::Value << "Notebook";
    yaml << YAML::Key << "preco" << YAML::Value << 3999.99;
    yaml << YAML::EndMap;

    // Salvando o arquivo
    std::ofstream fout("produto.yml");
    fout << yaml.c_str();

    // Carregando o arquivo
    YAML::Node produto = YAML::LoadFile("produto.yml");

    // Imprimindo na tela
    std::cout << "Produto: " << produto["produto"].as<std::string>() << std::endl;
    std::cout << "Preço: R$" << produto["preco"].as<double>() << std::endl;

    return 0;
}
```

A saída desse código seria:

```C++
Produto: Notebook
Preço: R$ 3999.99
```

Com isso, você já pode começar a trabalhar com YAML em seus projetos em C++.

## Mergulho Profundo

Além de salvar e carregar arquivos YAML, a biblioteca "yaml-cpp" oferece muitos outros recursos, incluindo a capacidade de manipular dados e estruturas YAML diretamente no código. Você pode criar uma árvore de nós YAML, adicionar elementos, acessar dados por meio de chaves e muito mais. A documentação oficial do "yaml-cpp" oferece uma lista completa de funcionalidades e exemplos de código.

Outra parte importante de trabalhar com YAML é entender sua sintaxe. YAML usa indentação e símbolos de dois pontos e traço para definir a estrutura e os dados no arquivo. É importante seguir a sintaxe correta para que seus arquivos YAML possam ser lidos corretamente por outros softwares.

Por último, mas não menos importante, é importante lembrar que o YAML é sensível a indentação e espaços em branco. Portanto, certifique-se de prestar atenção a esses detalhes ao criar e modificar seus arquivos YAML.

# Veja Também

- [Documentação oficial do "yaml-cpp"](https://github.com/jbeder/yaml-cpp/wiki)
- [Exemplos práticos de como usar YAML em C++](https://www.codeproject.com/Articles/1216565/Experiences-in-Using-YAML-for-Cplusplus)
- [Introdução ao YAML](https://www.datacamp.com/community/tutorials/reading-writing-files-yaml)