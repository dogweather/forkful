---
title:    "C#: テストの書き方"
keywords: ["C#"]
---

{{< edit_this_page >}}

## なぜテストを書くのか
ソフトウェア開発において、コードの品質を保証するためにテストを書くことは非常に重要です。テストを行うことで、バグを発見し修正することができ、安定性を確保することができます。

## テストを書く方法
```C#
using System;

// 単純な足し算関数のテスト
public int Add(int a, int b) {
    return a + b;
}

// Add関数のテストを行うためのコード
public void TestAdd() {
    int result = Add(2, 5);
    if(result == 7) {
        Console.WriteLine("Test Passed");
    } else {
        Console.WriteLine("Test Failed");
    }
}
```

上記の例では、単純な足し算関数`Add()`のテストを行っています。まず`Add()`関数を定義し、その後`TestAdd()`関数でテストを行っています。`TestAdd()`関数では、2と5を引数に渡し、その結果が7であるかを確認しています。もし結果が7であれば、「Test Passed」と表示され、そうでない場合は「Test Failed」と表示されます。このように、各関数やメソッドを細かくテストすることで、バグを早期に発見して修正することができます。

## テストの詳細
テストを書く際には、いくつかの原則に従うことが重要です。まず、テストはコード内で最初から計画し、同時に書く必要があります。また、テストは入力を変えたり、異なる状況下でコードを実行することで、カバレッジを高める必要があります。さらに、単体テストを行うことで、コードの依存関係を把握し、より複雑なプロジェクトでもテストを拡張しやすくなるでしょう。

## See Also
- [単体テストの重要性について](https://docs.microsoft.com/ja-jp/visualstudio/test/unit-tests/unit-tests-in-visual-studio?view=vs-2019)
- [C#のテストを書く方法](https://docs.microsoft.com/ja-jp/dotnet/core/testing/?view=aspnetcore-3.1)
- [xUnitを使ったテストの書き方](https://xunit.net/#write-tests)