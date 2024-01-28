---
title:                "Registro de Logs"
date:                  2024-01-26T01:00:09.045190-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Logs"
programming_language: "C++"
category:             "C++"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/logging.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?
A "logging" em programação é o processo de registrar eventos, estados e informações em um arquivo ou em outro meio de saída. Os programadores fazem logs para acompanhar o que está acontecendo em suas aplicações, para depurar problemas e para monitorar o desempenho para futuras análises e otimizações.

## Como:
Vamos supor que você esteja trabalhando em um sistema Linux e queira jogar seus logs em um arquivo com o bom e velho C++. Você vai querer incluir as bibliotecas `<iostream>` e `<fstream>` para realizar operações com arquivos. Aqui está um exemplo rápido:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ofstream logFile("appLog.txt", std::ios::app);  // Abrir no modo de anexação

    if (!logFile.is_open()) {
        std::cerr << "Houve um problema ao abrir o arquivo de log!" << std::endl;
        return 1;
    }

    logFile << "Aplicativo iniciado" << std::endl;
  
    // ... em algum lugar da lógica do seu aplicativo
    logFile << "Um evento importante ocorreu" << std::endl;

    // Não esqueça de fechar seu fluxo de arquivo
    logFile.close();

    return 0;
}
```

Se você acompanhar o arquivo de log com `tail -f appLog.txt`, deverá ver:

```
Aplicativo iniciado
Um evento importante ocorreu
```

Ótimo, você tem um registro cronológico de eventos!

## Aprofundamento
A "logging" é tão antiga quanto a computação em si, com raízes em marcas literais em papel para rastrear o que os antigos computadores estavam fazendo. Na era moderna, trata-se de soluções de software sofisticadas. Existem métodos de "logging" direto para arquivo, como o exemplo rápido e sujo acima, ou você pode se dar ao luxo de usar um framework de "logging" mais sofisticado, como Log4cpp ou Boost.Log no mundo C++; esses caras oferecem níveis de registro, controle de formato e mais.

Falando em níveis, as melhores práticas de "logging" incluem o uso de diferentes níveis de gravidade—informações, depuração, advertência, erro, fatal—para que você possa filtrar o ruído quando estiver tentando eliminar bugs ou tentar entender por que seu aplicativo está se comportando como um adolescente mal-humorado.

Quanto à questão do desempenho, não descuide dos seus logs. Um "logging" excessivo pode transformar seu aplicativo super rápido em uma maratona de lesmas, sobrecarregar sistemas de arquivos ou até custar dinheiro em taxas de armazenamento se você estiver baseado na nuvem. Encontrar o equilíbrio certo é fundamental: registre o que você precisa e nada mais.

## Veja Também
Para aqueles que gostam de ir além com suas práticas de "logging", confiram:

- A [Biblioteca Boost.Log](https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html) para alguns recursos de "logging" de alto nível.
- [A biblioteca glog do Google](https://github.com/google/glog) se você está interessado em como os gigantes da tecnologia estão usando para registrar seus aplicativos.
- [A biblioteca Log4cpp](http://log4cpp.sourceforge.net/) para um mecanismo de "logging" configurável.

E para uma leitura adicional sobre os porquês e os comos do "logging", mergulhe em:

- Esta discussão no Stack Overflow sobre [melhores práticas de "logging"](https://stackoverflow.com/questions/783956/logging-best-practices) que oferecerá um mergulho profundo revisado por pares no assunto.
