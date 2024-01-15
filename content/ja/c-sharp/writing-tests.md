---
title:                "テスト作成"
html_title:           "C#: テスト作成"
simple_title:         "テスト作成"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## なぜ

テストを書くことの重要性を実感したことがありますか？テストを書くことによって、コードの品質を向上させ、バグを事前に発見することができます。テストを書くことは、プログラミングの中でも特に重要なスキルです。

## 作り方

テストに関する基本的な考え方は、入力と出力が正しいかどうかを検証するということです。C#では、単体テストと統合テストの2種類のテストがあります。

単体テストの例を以下に示します。まずはテスト対象のクラスを用意します。

```
// テスト対象のクラス
public class Calculator
{
    // 2つの数値の和を返すメソッド
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

次に、xUnitフレームワークを使ってテストコードを書きます。テストコードは通常、テスト対象のクラス名に「Tests」をつけて命名します。

```
// テスト対象のクラス名に「Tests」をつけて命名するのが一般的
public class CalculatorTests
{
    // テストメソッドには「Fact」属性を付ける
    [Fact]
    public void Add_ReturnsCorrectSum()
    {
        // テスト対象のインスタンスを作成
        var calculator = new Calculator();

        // テスト対象のメソッドを実行
        var result = calculator.Add(5, 10);

        // 結果が期待通りかどうか検証
        Assert.Equal(15, result);
    }
}
```

統合テストの例も見てみましょう。統合テストは複数のクラスやメソッドを一度にテストすることで、アプリケーション全体の動作を確認するのに役立ちます。

```
// 統合テストのクラス
public class AppTests
{
    // Web APIのテストをする場合、クライアント側の処理も含めてテストするのが一般的
    [Fact]
    public void GetWeather_ReturnsCorrectData()
    {
        // テスト対象のWeb APIにリクエストを送信
        var response = await HttpClient.GetAsync("https://example.com/weather");

        // 結果が期待通りかどうか検証
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);

        // 取得した天気データをテスト
        var data = await response.Content.ReadAsAsync<WeatherData>();
        Assert.Equal("sunny", data.Condition);
        Assert.Equal(25, data.Temperature);
    }
}
```

## ディープダイブ

テストを書くことは簡単なように見えて、実際には注意すべきポイントがいくつかあります。例えば、テストケースが十分にカバーしているかどうか、テスト対象の挙動に依存しているテストは避けることなどが挙げられます。また、モックと呼ばれる仮のオブジェクトを使って、テスト対象の外部の依存性を取り除くことで、テストをより確実にすることができます。テストを書く際には、これらのポイントを意識することが重要です。

## See Also

- [C#での単体テストの書き方（Qiita）](https://qiita.com/ikaKou