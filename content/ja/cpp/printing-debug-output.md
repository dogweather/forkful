---
title:                "C++: デバッグ出力をプリントする"
simple_title:         "デバッグ出力をプリントする"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## 理由
デバッグ出力を印刷する理由は、プログラミングエラーを追跡し、修正するために重要です。デバッグ出力は、コードの特定のポイントで実行時の値を表示することができ、プログラムの動作を追跡するのに役立ちます。

## 方法
デバッグ情報を出力するには、 `cout` を使用し、 `<<` 演算子を使用して変数やメッセージを出力します。例えば、以下のようにコードを書くことができます。

```C++
#include <iostream>

using namespace std;

int main() {
    int num = 5;
    cout << "The value of num is: " << num << endl;
    return 0;
}
```

上記のコードでは、 `cout` を使用して `num` 変数の値を出力し、 `endl` を使用して改行します。実行すると、次のような出力が得られます。

```
The value of num is: 5
```

これで、実行時の値を確認することができます。

## 深堀り
デバッグ出力は、プログラムの実行中に特定のポイントでコードの値を確認することができるため、非常に便利です。また、デバッグ出力を使用することで、コード内のロジックを検証することもできます。ただし、デバッグ出力は本番環境で使用するのではなく、デバッグ目的でのみ使用することが推奨されます。

## 参考リンク
- [C++ 公式ドキュメント: 入出力](https://docs.microsoft.com/ja-jp/cpp/cpp/input-and-output?view=msvc-160)
- [プロキシティのエラーメッセージを読む方法：デバッグ出力](https://www.proxytype.jp/blog/how-to-read-error-messages-debug-output/)