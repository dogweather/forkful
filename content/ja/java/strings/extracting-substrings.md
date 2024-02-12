---
title:                "部分文字列の抽出"
aliases:
- /ja/java/extracting-substrings/
date:                  2024-01-20T17:45:54.352333-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
サブストリング抽出とは文字列の一部を取り出すことです。この操作はデータの解析やフォーマット変更時によく使われます。

## How to:
Javaでは `substring` メソッドを使ってサブストリングを簡単に取り出せます。例を見てみましょう。

```java
public class SubstringExample {
    public static void main(String[] args) {
        String fullString = "こんにちは、世界！";
        String greeting = fullString.substring(0, 5); // "こんにちは" を取り出す
        
        System.out.println(greeting); // "こんにちは" を表示
    }
}
```

出力:
```
こんにちは
```

## Deep Dive
Javaにおける `substring` メソッドはJava 1から存在しています。Java 1.4までは文字列を実際にコピーしていましたが、メモリ使用量の問題から、Java 7では新しい文字列を作る方式に変わりました。`substring` の代わりに `String` クラスの `split`, `charAt`, そして `pattern` クラスを使うことも可能ですが、複雑な操作になる場合は、`substring` が便利です。

## See Also
- OracleのJavaドキュメント: [String (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- 文字列操作に関する詳細なガイド: [Baeldung - Guide to String](https://www.baeldung.com/java-string)
