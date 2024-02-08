---
title:                "Descobrindo o comprimento de uma string"
aliases:
- pt/cpp/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:09.986891-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descobrindo o comprimento de uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Não há enrolação: calcular o tamanho de uma string significa descobrir quantos caracteres ela tem. Fazemos isso para manipular texto com precisão, seja para slice, comparar ou validar dados.

## How to:
C++ oferece métodos simples para essa tarefa. Vamos usar `length()` e `size()` — sim, os dois funcionam igual no mundo das strings. Confira:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string minha_string = "Olá, programadores!";
    cout << "Tamanho usando length(): " << minha_string.length() << endl;
    cout << "Tamanho usando size(): " << minha_string.size() << endl;
    return 0;
}
```

Saída:

```
Tamanho usando length(): 20
Tamanho usando size(): 20
```

Ambos dizem quantos caracteres temos. Simples assim.

## Deep Dive
O negócio dos tamanhos vem de longe. Na era do C, `strlen()` fazia o serviço, mas iterando até achar o terminador nulo `'\0'`. Hoje, com C++, `string::length()` e `string::size()` são sinônimos e retornam um `size_type`. Eles são rápidos porque a `std::string` guarda seu tamanho.

Alternativas? Podemos falar de `std::string_view` em C++17, que dá um jeito de ver strings sem copiar. E tem o `std::strlen()` da biblioteca `<cstring>`, herança do C.

No caso da implementação, `std::string` tem a eficiência incorporada. Não é a busca cega de antigamente. Saber o tamanho é instantâneo, é parte da estrutura da string, então tanto `length()` quanto `size()` são O(1).

## See Also
Para uma olhada mais aprofundada nas entranhas das strings em C++:

- C++ Reference para `std::string::size`: https://en.cppreference.com/w/cpp/string/basic_string/size
- C++ Reference para `std::string::length`: https://en.cppreference.com/w/cpp/string/basic_string/length
- Um artigo detalhado sobre `std::string` e `std::string_view`: https://www.modernescpp.com/index.php/c-core-guidelines-string
- Para os curiosos sobre a evolução do C++ e suas strings: https://isocpp.org/wiki/faq/cpp-migration#char-strings-safe
