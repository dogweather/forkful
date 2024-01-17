---
title:                "Verificando se um diretório existe"
html_title:           "Arduino: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# O que e por quê?
Verificar se um diretório existe é um processo importante para garantir a funcionalidade correta do seu programa. É uma ação comumente praticada por programadores para garantir que os arquivos necessários para o funcionamento do programa estejam presentes no local correto.

# Como fazer:
```Arduino
if (SD.exists("diretorio")) {
  Serial.println("O diretório existe!");
} else {
  Serial.println("O diretório não existe!");
}
```

# Explorando mais a fundo:
Verificar a existência de um diretório pode ser útil em diferentes situações, como por exemplo, checar se um dispositivo de armazenamento está conectado ao Arduino. Existem outras formas de realizar essa verificação, como utilizando a biblioteca `File` ou o comando `listFiles()`.

# Veja também:
- Documentação oficial do Arduino sobre a função [exists()](https://www.arduino.cc/en/Reference/Exists)
- Tutorial sobre a biblioteca `File` do Arduino [aqui](https://www.arduino.cc/en/Tutorial/LibraryExamples/Listfiles)
- Mais informações sobre a função `listFiles()` [aqui](https://arduinogetstarted.com/tutorials/arduino-list-files)