---
title:                "デバッガーの使い方"
aliases: - /ja/swift/using-a-debugger.md
date:                  2024-01-26T04:10:56.786043-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガを使用するということは、実行中のコードをテストし、検査するための特殊なツールを活用することを意味します。これは大きな意味があります。なぜなら、プログラムの内部で何が起こっているかを見ることができ、バグを見つけ、コードの動作をよりよく理解できるからです。

## 使い方：
SwiftのIDEであるXcodeでデバッガを使用するには、ブレークポイントを設定し、変数を調べ、式を監視できます。例を見てみましょう：

```Swift
func findFactorial(of number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(of: number - 1)
}

let result = findFactorial(of: 5)
print(result)
```

Xcodeで行番号の左をクリックしてブレークポイントを設定し、プログラムを実行します。ブレークポイントに到達すると、Xcodeは実行を一時停止します。これで以下を行うことができます：

1. 変数の値をチェックする。
2. デバッガのコントロールを使用して、次の行を実行（ステップオーバー）するか、関数内に入る（ステップイン）。
3. 特定の変数や定数の変更を監視するために「監視リスト」に式を追加する。

デバッグ領域で以下のようなことが見られるかもしれません：

```
(lldb) po number
5
(lldb) po result
120
```

## 深掘り：
デバッガは、1940年代からプログラミングの風景の一部となっており、単純なブレークポイントシステムから複雑なUI操作体験まで進化してきました。Xcodeの組み込みデバッガ以外の選択肢には、Xcodeが内部で使用しているLLDB（Low Level Debugger）のようなサードパーティツールが含まれます。一部の人々は、`print()`ステートメントを使用してデバッグを行います（俗に「洞窟人デバッギング」と呼ばれますが）、これは大規模なプロジェクトや複雑なバグには効率的ではありません。デバッガを使用するとき、実行制御、ランタイムの内省、およびデータ操作を扱っています。これらの原則を深く理解することは、効率的なデバッグに大きく寄与します。

## 参照：
- [AppleのXcodeデバッグガイド](https://developer.apple.com/documentation/xcode/debugging/)
- [LLDBクイックスタートガイド](https://lldb.llvm.org/use/tutorial.html)
- [Ray WenderlichのSwiftデバッグチュートリアル](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
