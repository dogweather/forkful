---
date: 2024-01-26 00:56:00.445607-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A Kotlin\u306F\u30A8\u30E9\u30FC\u3092\
  \u7BA1\u7406\u3059\u308B\u305F\u3081\u306B`try`, `catch`, `finally`, `throw`\u3092\
  \u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u3053\u3061\u3089\u304C\u4F7F\u3044\
  \u65B9\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.075176-06:00'
model: gpt-4-1106-preview
summary: "Kotlin\u306F\u30A8\u30E9\u30FC\u3092\u7BA1\u7406\u3059\u308B\u305F\u3081\
  \u306B`try`, `catch`, `finally`, `throw`\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\
  \u3059\u3002\u3053\u3061\u3089\u304C\u4F7F\u3044\u65B9\u3067\u3059\uFF1A."
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

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
