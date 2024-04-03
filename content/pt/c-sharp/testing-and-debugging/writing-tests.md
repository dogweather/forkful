---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:28.929083-07:00
description: "Como fazer: Desenvolvedores C# usam principalmente os frameworks NUnit\
  \ ou xUnit para escrever testes devido \xE0 sua flexibilidade e conjunto extensivo\
  \ de\u2026"
lastmod: '2024-03-13T22:44:46.587569-06:00'
model: gpt-4-0125-preview
summary: "Desenvolvedores C# usam principalmente os frameworks NUnit ou xUnit para\
  \ escrever testes devido \xE0 sua flexibilidade e conjunto extensivo de recursos."
title: Escrevendo testes
weight: 36
---

## Como fazer:
Desenvolvedores C# usam principalmente os frameworks NUnit ou xUnit para escrever testes devido à sua flexibilidade e conjunto extensivo de recursos. Aqui está um exemplo básico usando o NUnit para testar uma simples função de adição:

1. **Instale o NUnit e o NUnit3TestAdapter** via Gerenciador de Pacotes NuGet ou o CLI .NET:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **Crie um projeto de biblioteca de classes C#** se você ainda não o fez.

3. **Escreva uma função simples** para testar. Por exemplo, um método de adição em uma classe chamada `Calculator`:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **Escreva uma classe de teste** usando NUnit:
```csharp
using NUnit.Framework;

namespace CalculatorTests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_SomaDoisInteiros_RetornaSomaCorreta()
        {
            // Arrange (Preparação)
            var calculator = new Calculator();
            int esperado = 5;

            // Act (Ação)
            int atual = calculator.Add(2, 3);

            // Assert (Verificação)
            Assert.AreEqual(esperado, atual);
        }
    }
}
```

5. **Execute o teste** usando o executor de testes da sua IDE ou o CLI .NET:
```powershell
dotnet test
```

### Saída de Exemplo:
Assumindo que seu teste passe, você deverá ver uma saída semelhante a esta:
```
Execução de Testes Bem-Sucedida.
Testes totais: 1
     Aprovados: 1
 Tempo total: 1.2345 Segundos
```

### Usando xUnit:
Se você prefere o xUnit, a configuração é semelhante ao NUnit. Aqui está como você reescreveria o exemplo de teste para a classe `Calculator` usando o xUnit:

1. **Instale o xUnit e o xUnit.runner.visualstudio**:
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **Escreva uma classe de teste usando o xUnit**:
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_SomaDoisInteiros_RetornaSomaCorreta()
        {
            // Arrange (Preparação)
            var calculator = new Calculator();
            int esperado = 5;

            // Act (Ação)
            int atual = calculator.Add(2, 3);

            // Assert (Verificação)
            Assert.Equal(esperado, atual);
        }
    }
}
```

3. **Execute o teste usando o CLI .NET** ou o executor de testes integrado da sua IDE.

Tanto o NUnit quanto o xUnit oferecem recursos poderosos para testes parametrizados, operações de configuração/encerramento e organização de testes em categorias, tornando-se ferramentas indispensáveis no kit de ferramentas do programador C# para garantir a qualidade e funcionalidade do código.
