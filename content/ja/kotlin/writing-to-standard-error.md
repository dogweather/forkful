---
title:    "Kotlin: 標準エラーへの書き込み"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## なぜ

プログラマーとして、私たちは常にコードを改善し、バグを修正することに注力しています。しかし、時には私たちのコードが予期しないエラーを発生させてしまいます。このような場合、私たちはデバッグ中に値を監視するために、スタンダードエラーへの書き込みを行うことができます。

## し方

スタンダードエラーに書き込むには、Kotlinの ```System.err.println()``` メソッドを使用します。以下はそのコーディング例です。

```
Kotlin fun main() {
    val number = 10
    if (number > 5) {
        System.err.println("数値は5より大きいです。")
    }
}
```

出力結果は以下のようになります。

```
数値は5より大きいです。
```

## 深層に潜る

スタンダードエラーに対する書き込みは、主にデバッグの目的で使用されます。しかし、実際には様々なシナリオで役立つことがあります。例えば、ログの出力先として使用することで、コード内の特定の箇所で何が起こっているかを把握することができます。また、異なるコマンドラインツールやシステム間でのコミュニケーションにも使用できます。

## 参考リンク

[Official Kotlin Documentation](https://kotlinlang.org/docs/reference) <br>
[Kotlin for Java Developers](https://www.udemy.com/course/kotlin-for-java-developers/) <br>
[Debugging with System.err in Kotlin](https://medium.com/@khandedeshmukh1994/debugging-with-system-err-in-kotlin-d1c8d1b11f6e)

## また見る

[JavaのSystem.outとSystem.errの違い](https://techplay.jp/column/591)