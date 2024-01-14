---
title:                "Arduino: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando no Arduino?

Se você deseja maximizar o potencial do seu projeto Arduino ou ter controle sobre as funcionalidades do seu dispositivo, ler argumentos de linha de comando pode ser uma ferramenta útil. Com essa habilidade, você pode criar um programa mais dinâmico e interativo.

## Como ler argumentos de linha de comando no Arduino?

Para ler argumentos de linha de comando no Arduino, você pode seguir os seguintes passos:

1. Inicie criando uma função `main` que receberá os argumentos como parâmetros:
```Arduino
void main(int argc, char *argv[]) {
    //seu código aqui
}
```

2. Em seguida, você deve criar um loop para navegar pelos argumentos:
```Arduino
for (int i = 0; i < argc; i++) {
    char *argumento = argv[i]; //salva cada argumento em uma variável
    //seu código aqui
}
```

3. Você pode utilizar a função `strcmp()` para comparar os argumentos recebidos com valores específicos e executar diferentes ações de acordo com cada um:
```Arduino
if (strcmp(argumento, "ligar") == 0) {
    //ação para ligar algo
}
```

## Aprofundando nos argumentos de linha de comando

Ler argumentos de linha de comando pode ser uma tarefa muito útil, especialmente quando se trata de desenvolver projetos mais complexos no Arduino. Com essa habilidade, é possível criar programas mais personalizados e adaptáveis às suas necessidades.

É importante ressaltar que os argumentos de linha de comando não devem ser utilizados como a única forma de interação com o dispositivo, mas sim como uma função adicional para tornar seu projeto mais versátil.

## Veja também

- [Documentação oficial do Arduino](https://www.arduino.cc/reference/en/language/functions/command-line-arguments/)
- [Tutorial sobre como ler argumentos de linha de comando no Arduino](https://www.makerguides.com/arduino-command-line-arguments/)
- [Vídeo explicativo sobre argumentos de linha de comando no Arduino](https://www.youtube.com/watch?v=YHLZBZ8Jr68)