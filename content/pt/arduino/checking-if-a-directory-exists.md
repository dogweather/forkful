---
title:                "Arduino: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Verificar se um diretório existe é um processo importante ao trabalhar com Arduino. Isso permite que o programa execute tarefas específicas com base na existência ou não de um diretório. Pode ser útil em situações onde o usuário precisa salvar ou recuperar dados de um diretório específico.

## Como fazer

Para verificar se um diretório existe, podemos usar a função `exists()` da biblioteca "SD.h". Esta função retorna um valor booleano verdadeiro se o diretório existir e falso se não existir. Aqui está um exemplo de como usar essa função:

```
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (SD.exists("meu_diretorio")) {      //verifica se o diretório "meu_diretorio" existe
    Serial.println("O diretório existe!");
  } else {
    Serial.println("O diretório não existe!");
  }
}

void loop() {
  
}
```

Se o diretório "meu_diretorio" existir, o programa imprimirá "O diretório existe!" no monitor serial. Caso contrário, ele imprimirá "O diretório não existe!".

## Aprofundando

Quando usamos a função `exists()` para verificar um diretório, o Arduino tenta acessar o diretório especificado e, se conseguir, o diretório é considerado como existente. Caso contrário, ele é considerado inexistente. É importante notar que esta função não verifica se o diretório contém arquivos, apenas se o diretório em si existe.

## Veja também

- Documentação da função exists() da biblioteca SD.h: https://www.arduino.cc/reference/en/libraries/sd/exist/
- Tutorial sobre o uso de diretórios no Arduino: https://www.arduino.cc/en/Tutorial/Files

O processo de verificar a existência de um diretório pode ser muito útil ao trabalhar com projetos mais complexos no Arduino. Com a compreensão deste processo, você pode utilizar essa função em seus projetos para facilitar o armazenamento e recuperação de dados. Então, verifique sempre se o diretório existe antes de tentar acessá-lo!