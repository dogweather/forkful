---
title:                "Iniciando um novo projeto"
html_title:           "Javascript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Iniciar um novo projeto em Arduino é criar um plano novo desde o início, abordando qualquer desafio de programação em nível microcontrolador. Programadores fazem isso para resolver problemas específicos com um controle granular dos componentos do hardware.

## Como Faz:
Veja um exemplo de um sketch (programa Arduino) simples com sua respectiva saída.

```Arduino
void setup() 
{
  Serial.begin(9600); 
}

void loop()
{
  Serial.println("Olá, Mundo!"); 
  delay(1000); 
}
```
Ao carregar esse código em seu Arduino, ele deve imprimir a frase "Olá, Mundo!" no Monitor Serial a cada segundo.

## Mergulho Profundo
O Arduino surgiu na Itália em 2005 para proporcionar aos hobbyistas e aos artistas um meio acessível e fácil de usar de incorporar a automação em seus projetos. Alternativas incluem o Raspberry Pi e o BeagleBone. Implementar um novo projeto em Arduino requer escrita de código, carregamento do sketch no hardware Arduino, e depuração. Os microcontroladores Arduino são conhecidos por sua flexibilidade, permitindo a implementação de uma variedade de projetos, desde sistemas de irrigação automatizados a robôs autônomos.

## Veja Também
* [Documentação Oficial do Arduino](https://www.arduino.cc/reference/en/)
* [Tutoriais Arduino no Programar Facil](https://www.programarfacil.com/blog/tutoriales-arduino/)
* [Livros Arduino na Amazon](https://www.amazon.com.br/Arduino/livros/s?k=Arduino)