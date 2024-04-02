---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:01.436897-07:00
description: "Escrever testes em Swift envolve criar e executar c\xF3digo que verifica\
  \ a corre\xE7\xE3o de outras unidades de c\xF3digo na sua aplica\xE7\xE3o. Programadores\
  \ fazem isso\u2026"
lastmod: '2024-03-13T22:44:46.923941-06:00'
model: gpt-4-0125-preview
summary: "Escrever testes em Swift envolve criar e executar c\xF3digo que verifica\
  \ a corre\xE7\xE3o de outras unidades de c\xF3digo na sua aplica\xE7\xE3o. Programadores\
  \ fazem isso\u2026"
title: Escrevendo testes
weight: 36
---

## O Que & Por Que?
Escrever testes em Swift envolve criar e executar código que verifica a correção de outras unidades de código na sua aplicação. Programadores fazem isso para assegurar a confiabilidade, detectar bugs cedo no ciclo de desenvolvimento, e facilitar o refatoramento de código futuro sem consequências não intencionais.

## Como fazer:
Swift suporta testes por meio de seu framework XCTest, que é integrado ao Xcode. Você pode escrever testes de unidade para verificar partes individuais do seu código, por exemplo, uma função que calcula a soma de dois números.

```swift
import XCTest
@testable import SuaApp

class TestesDaSuaApp: XCTestCase {

    func testaSoma() {
        let resultado = Calculadora().soma(a: 1, b: 2)
        XCTAssertEqual(resultado, 3, "A função de soma não retornou o valor esperado.")
    }
}
```

Para rodar este teste, você normalmente pressionaria Command-U no Xcode. A saída no navegador de testes do Xcode dirá se o teste foi aprovado ou reprovado.

Por exemplo, uma saída de teste bem-sucedida:
```
Test Case '-[TestesDaSuaApp testaSoma]' passed (0.005 seconds).
```

Para cenários de teste mais avançados, você pode adotar bibliotecas de terceiros como Quick/Nimble, que oferecem uma sintaxe mais expressiva para escrever testes.

Com Quick/Nimble, você poderia escrever o mesmo teste assim:

```swift
// Adicione Quick e Nimble ao seu gerenciador de pacotes Swift ou use CocoaPods/Carthage para instalá-los
import Quick
import Nimble
@testable import SuaApp

class EspecCalculadora: QuickSpec {
    override func spec() {
        describe("Calculadora") {
            context("ao somar números") {
                it("deve retornar a soma correta") {
                    let calculadora = Calculadora()
                    expect(calculadora.soma(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

Rodar este teste daria uma saída similar no seu console de teste ou log da ferramenta de CI/CD, indicando se o teste foi bem-sucedido ou não, com um formato mais legível para descrever testes e expectativas.
