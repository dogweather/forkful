---
aliases:
- /pt/java/using-a-debugger/
date: 2024-01-26 03:49:42.207726-07:00
description: "Usar um depurador significa empregar uma ferramenta para testar e corrigir\
  \ bugs em seu c\xF3digo. Os programadores fazem isso para entender o fluxo de suas\u2026"
lastmod: 2024-02-18 23:08:58.023215
model: gpt-4-0125-preview
summary: "Usar um depurador significa empregar uma ferramenta para testar e corrigir\
  \ bugs em seu c\xF3digo. Os programadores fazem isso para entender o fluxo de suas\u2026"
title: Usando um depurador
---

{{< edit_this_page >}}

## O Que & Por Quê?
Usar um depurador significa empregar uma ferramenta para testar e corrigir bugs em seu código. Os programadores fazem isso para entender o fluxo de suas aplicações, identificar as fontes de erros e verificar a lógica em execução.

## Como Fazer:
Digamos que você tenha um simples programa Java que está causando problemas, e você não consegue descobrir o porquê. Aqui está como você iniciaria um depurador usando o Eclipse, um dos IDEs populares para desenvolvimento Java:

Primeiro, certifique-se de que você definiu um ponto de interrupção. Em seguida, clique com o botão direito no arquivo, selecione 'Depurar Como', e clique em 'Aplicação Java'.

```Java
public class ExemploDebug {
    public static void main(String[] args) {
        int a = 5;
        int b = 0;
        // Defina um ponto de interrupção aqui
        int resultado = dividir(a, b);
        System.out.println("O resultado é: " + resultado);
    }

    private static int dividir(int numerador, int denominador) {
        // Outro bom local para um ponto de interrupção
        return numerador / denominador;
    }
}
```

Fazendo isso, seu programa pausará no ponto de interrupção, e você poderá inspecionar variáveis, avançar através do código linha por linha e observar como seu programa se comporta.

Saída de Exemplo (em um console de depurador):
```
Ponto de interrupção atingido em: int resultado = dividir(a, b);
```

## Aprofundando
O conceito de depuração existe desde os primeiros dias da programação. Há uma lenda de que o termo "bug" veio de um inseto real encontrado dentro de um computador por Grace Hopper, pioneira na área. Avançando para hoje, temos IDEs sofisticados como IntelliJ IDEA, Eclipse e NetBeans que possuem depuradores poderosos.

Alternativas aos depuradores de IDE incluem logging, instruções de impressão (depurador dos pobres), asserções e ferramentas de depuração independentes como jdb (Java Debugger), que faz parte do Java Development Kit (JDK).

Um depurador funciona permitindo que o programador pause a execução (pontos de interrupção), avance pelo código, inspecione valores de variáveis, modifique esses valores em tempo real e até execute blocos de código um a um. O uso de um depurador é frequentemente considerado uma técnica inestimável para desenvolver aplicações complexas, onde rastrear a linha exata de código causando um problema pode ser comparado a encontrar uma agulha no palheiro.

## Veja Também
- A documentação oficial da Oracle sobre depuração: [Depuração Oracle Java SE](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)
- Guia do Eclipse sobre depuração: [Dicas de Depuração do Eclipse](https://www.eclipse.org/community/eclipse_newsletter/2017/june/article4.php)
- VisualVM, uma ferramenta visual que integra várias ferramentas de linha de comando do JDK e capacidades de profiling leves: [VisualVM](https://visualvm.github.io/)
