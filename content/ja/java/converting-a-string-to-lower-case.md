---
title:                "文字列を小文字に変換"
aliases:
- ja/java/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:47.827596-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？とは？)
文字列を小文字に変換するとは、すべての大文字を対応する小文字に変える処理のことです。検索、ソート、一貫性のあるデータ保存などのため、プログラマーはよくこの操作を行います。

## How to: (方法)
Javaで文字列を小文字に変換する簡単な例を見てみましょう。

```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String original = "Kon'nichiwa, SEKAI!";
        String lowerCased = original.toLowerCase();
        System.out.println(lowerCased);
    }
}
```
出力:
```
kon'nichiwa, sekai!
```

## Deep Dive (掘り下げ)
Javaでは、`String` クラスの `toLowerCase()` メソッドで小文字変換を行います。初期のJavaバージョンから存在し、多言語対応を強化するために何度も改善されてきました。

**代替手法:** `toLowerCase()` にはロケールを指定できるオーバーロードがあります。例えば、`toLowerCase(Locale.ROOT)` は言語環境に依存しない結果を返すので、言語に敏感なデータで使われます。

**実装の詳細:** 内部的に `toLowerCase()` は文字コード表を使い、各文字を対応する小文字にマッピングします。Unicode規格に従い、特定言語の追加ルールを適用することもあります。

## See Also (関連情報)
- [Java String toLowerCase() Method](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase())
- [Unicode Case Mapping](https://www.unicode.org/reports/tr21/tr21-5.html)
- [Locale-Specific Lowercase Conversion](https://docs.oracle.com/javase/tutorial/i18n/locale/toLowerCase.html)
