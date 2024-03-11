---
date: 2024-01-26 00:56:00.445607-07:00
description: "\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u3001\u5B9F\u884C\u4E2D\u306B\u767A\
  \u751F\u3059\u308B\u554F\u984C\u306B\u30B3\u30FC\u30C9\u304C\u3069\u306E\u3088\u3046\
  \u306B\u5BFE\u51E6\u3059\u308B\u304B\u3067\u3059\u2500\u2500\u4F8B\u3048\u3070\u3001\
  \u30AB\u30FC\u30D6\u30DC\u30FC\u30EB\u3092\u53D6\u308A\u3053\u307C\u3055\u305A\u306B\
  \u51E6\u7406\u3059\u308B\u3088\u3046\u306A\u3082\u306E\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30AF\u30E9\u30C3\u30B7\u30E5\u3092\u9632\u304E\
  \u3001\u30E6\u30FC\u30B6\u30FC\u306B\u30B9\u30E0\u30FC\u30BA\u306A\u4F53\u9A13\u3092\
  \u63D0\u4F9B\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
lastmod: '2024-03-11T00:14:15.655748-06:00'
model: gpt-4-1106-preview
summary: "\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u3001\u5B9F\u884C\u4E2D\u306B\u767A\
  \u751F\u3059\u308B\u554F\u984C\u306B\u30B3\u30FC\u30C9\u304C\u3069\u306E\u3088\u3046\
  \u306B\u5BFE\u51E6\u3059\u308B\u304B\u3067\u3059\u2500\u2500\u4F8B\u3048\u3070\u3001\
  \u30AB\u30FC\u30D6\u30DC\u30FC\u30EB\u3092\u53D6\u308A\u3053\u307C\u3055\u305A\u306B\
  \u51E6\u7406\u3059\u308B\u3088\u3046\u306A\u3082\u306E\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30AF\u30E9\u30C3\u30B7\u30E5\u3092\u9632\u304E\
  \u3001\u30E6\u30FC\u30B6\u30FC\u306B\u30B9\u30E0\u30FC\u30BA\u306A\u4F53\u9A13\u3092\
  \u63D0\u4F9B\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
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
