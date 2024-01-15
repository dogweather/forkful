---
title:                "编写测试"
html_title:           "C#: 编写测试"
simple_title:         "编写测试"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

為什麼： 很多人可能會覺得寫測試很無聊或浪費時間，但是實際上，寫測試可以幫助我們更有效率地開發程式，並降低出錯的機率。

## 為什麼要寫測試？

寫測試可以幫助我們驗證程式碼是否符合預期功能，並且在程式碼改動後避免出現意外的錯誤。同時，寫測試可以幫助我們更快速地定位問題，提高程式碼的可靠性和可讀性。

## 如何寫測試？

```C#
// 假設我們有一個計算兩個數字相加的方法
public int Add(int num1, int num2)
{
    return num1 + num2;
}

// 使用測試框架 NUnit 進行測試
[TestFixture]
public class CalculatorTests
{
    [Test]
    public void TestAdd()
    {
        // Arrange - 初始化測試所需的參數
        var calculator = new Calculator();
        int num1 = 2;
        int num2 = 3;

        // Act - 呼叫要測試的方法
        int result = calculator.Add(num1, num2);

        // Assert - 驗證方法的回傳結果與預期是否相符
        Assert.AreEqual(5, result);
    }
}
```

## 深入探討寫測試

除了使用測試框架外，我們也可以手動撰寫測試程式碼，例如使用 `Console.WriteLine()` 來輸出結果。另外，也可以使用 [Moq](https://github.com/moq/moq4) 等工具來模擬特定的環境和物件，進行更全面的測試。在寫測試時，也要注意測試的範圍，避免過於龐大和冗雜的測試，以及盡量避免重複測試。

## 參考資料

- [NUnit](https://github.com/nunit/nunit)
- [Moq](https://github.com/moq/moq4)
- [微軟：.NET 測試技術入門](https://docs.microsoft.com/zh-tw/dotnet/core/testing/)