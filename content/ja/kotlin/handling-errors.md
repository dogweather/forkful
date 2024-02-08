---
title:                "エラー処理"
date:                  2024-01-26T00:56:00.445607-07:00
model:                 gpt-4-1106-preview
simple_title:         "エラー処理"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/handling-errors.md"
---

{{< edit_this_page >}}

## 何となぜ？
エラー処理は、実行中に発生する問題にコードがどのように対処するかです──例えば、カーブボールを取りこぼさずに処理するようなものです。プログラマーは、クラッシュを防ぎ、ユーザーにスムーズな体験を提供するためにこれを行います。

## どのように：
Kotlinはエラーを管理するために`try`, `catch`, `finally`, `throw`を提供しています。こちらが使い方です：

```Kotlin
fun main() {
    val numerator = 10
    val denominator = 0

    try {
        val result = numerator / denominator
        println("Result: $result")
    } catch (e: ArithmeticException) {
        println("Can't divide by zero, buddy.")
    } finally {
        println("This happens no matter what.")
    }
}
```

出力：
```
Can't divide by zero, buddy.
This happens no matter what.
```

`try`ブロック内で何かがうまく行かない場合、実行は`catch`にジャンプします。これはスローされた特定のエラー（この場合は`ArithmeticException`）をキャッチします。`finally`ブロックは、結果にかかわらずその後に実行されます。

## 深掘り
`try-catch`ブロックは初期のプログラミングの時代からあります──安全網のようなものです。Kotlinには、例外を自分で投げ込むための`throw`があり、必ず実行したいコードのための`finally`（よくある後始末の作業）があります。

選択肢には`Result`タイプやKotlinの`try`を式として扱う方法があります。

```Kotlin
val result: Result<Int> = try {
    Result.success(numerator / denominator)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
このアプローチは`Result`オブジェクトを返します──ハンドルされない例外のドラマなしに、成功か失敗を得ることができます。

Kotlinでの実装は、`try`を式として使えるためすっきりしています。つまり、値を返します。このような選択肢は、Kotlinでのエラー処理をかなり多様にします。ワークショップで道具を選ぶように、仕事に適した道具を選ぶことについてです。

## 関連情報
- 例外に関するKotlinドキュメント：[Kotlinの例外処理](https://kotlinlang.org/docs/exception-handling.html)
- `Result`タイプに関するKotlinドキュメント：[Kotlin Result](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- 『Effective Java（第3版）』ジョシュア・ブロック著 — 例外に関する素晴らしい洞察を提供していますが、Javaに特化しています。
