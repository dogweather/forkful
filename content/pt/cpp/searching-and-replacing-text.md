---
title:    "C++: Buscando e substituindo texto"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que?

Se você é um programador, provavelmente já se deparou com a tarefa de procurar e substituir textos em seu código. Isso pode ser necessário para corrigir erros ortográficos, atualizar informações ou até mesmo fazer alterações em massa. Felizmente, a linguagem de programação C++ possui recursos integrados para facilitar essa tarefa.

## Como fazer?

Para procurar e substituir texto em C++, você pode usar a função `find_and_replace()` da biblioteca `<algorithm>`. Veja um exemplo de código abaixo:

```
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

// Função para procurar e substituir texto
void find_and_replace(string& texto, const string& buscar, const string& substituir) {
    size_t pos = texto.find(buscar);
    while (pos != string::npos) {
        texto.replace(pos, buscar.length(), substituir);
        pos = texto.find(buscar, pos + substituir.length());
    }
}

int main() {
    string texto = "Este é um exemplo de texto para substituir.";
    cout << "Texto original: " << texto << endl;

    // Chamando a função para substituir 'exemplo' por 'exemplo novo'
    find_and_replace(texto, "exemplo", "exemplo novo");
    cout << "Texto modificado: " << texto << endl;

    return 0;
}
```

**Saída:**

```
Texto original: Este é um exemplo de texto para substituir.
Texto modificado: Este é um exemplo novo de texto para substituir.
```

Este exemplo usa a função `find_and_replace()` para procurar e substituir a palavra "exemplo" por "exemplo novo" no texto. A função atualiza a posição da palavra a cada substituição, permitindo que o texto seja percorrido completamente.

## Aprofundando

Além da função `find_and_replace()`, a biblioteca `<algorithm>` também possui outras funções úteis para manipular strings, como `find()`, `replace()`, `erase()`, entre outras. Você pode combiná-las para criar algoritmos mais complexos de busca e substituição.

Além disso, vale ressaltar que é importante ter cuidado ao utilizar esses recursos, pois eles podem substituir não apenas as ocorrências desejadas, mas também outras que possam causar problemas no seu código. Por isso, sempre faça testes e verifique se a substituição foi realizada corretamente.

## Veja também

- [Documentação oficial da função `find_and_replace()`](https://en.cppreference.com/w/cpp/algorithm/find_and_replace)
- [Guia completo sobre a manipulação de strings em C++](https://www.geeksforgeeks.org/strings-c-3/)
- [Vídeo aula sobre busca e substituição em C++](https://www.youtube.com/watch?v=Oyc3ybWx268)