---
title:                "文字列の補間"
html_title:           "Kotlin: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## これは何？

文字列補間とは、「```${variable}```」のように、文字列内に変数の値を埋め込むことです。プログラマーは、特定のテキストを組み立てる際に、変数の値を動的に挿入するために文字列補間を使用します。

## 方法：

**例1：**

```Kotlin
val name = "太郎"
val message = "こんにちは、${name}さん！"
println(message)
```

**出力：**
```
こんにちは、太郎さん！
```

**例2：**

```Kotlin
val age = 20
val message = "あなたの年齢は${age}歳です。"
println(message)
```

**出力：**
```
あなたの年齢は20歳です。
```

## 詳細

- 文字列補間は、Kotlinの機能の一部ではなく、プログラマーによって開発されたライブラリであるtextフォーマットマイナーで初めて使用されました。
- 代替として、プログラマーは文字列結合演算子「+」を使用することができますが、文字列補間を使用するとより簡潔で読みやすいコードが書けます。
- 実装の詳細は、文字列内に特殊な文字（例：シングルクォーテーションやバックスラッシュ）が使用されている場合、エスケープシーケンスを使用してこれらの文字をエスケープする必要があります。

## 関連リンク

- [Kotlinドキュメント](https://kotlinlang.org/docs/strings.html#string-interpolation)
- [プログラミング用語集](https://www.javadrive.jp/app/string/index6.html)
- [テキストフォーマットマイナーの公式サイト](https://textformatmine.com/)