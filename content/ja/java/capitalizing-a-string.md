---
title:                "文字列の先頭を大文字にする"
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を大文字にすることは、文字列の各単語の最初の文字を大文字に変換することです。プログラマは一貫性ある表示を保ったり、タイトルや見出しを適切にフォーマットしたりするためにこれを行います。

## How to: (方法)
Javaで文字列を大文字にする一般的な方法は、`toUpperCase()` メソッドもしくは Apache Commons Lang の `WordUtils.capitalize()` を使用することです。以下に例を示します。

```java
public class CapitalizeExample {
    public static void main(String[] args) {
        // Using String.toUpperCase() to capitalize the entire string
        String message = "hello world";
        String capitalized = message.toUpperCase();
        System.out.println(capitalized); // Output: HELLO WORLD

        // Using WordUtils.capitalize() from Apache Commons Lang to capitalize each word
        // Add the dependency: org.apache.commons:commons-lang3:3.12.0
        String title = "the lord of the rings";
        String bookTitle = org.apache.commons.text.WordUtils.capitalize(title);
        System.out.println(bookTitle); // Output: The Lord Of The Rings
    }
}
```

## Deep Dive (掘り下げ)
歴史的には、Javaは文字列操作の基本機能を提供してきました。`toUpperCase()` はその一例で、全ての文字を大文字にします。しかし、各単語の先頭文字だけを大文字にするには、外部ライブラリを使ったり、独自のロジックを書いたりする必要があります。

Apache Commons Langライブラリの `WordUtils.capitalize()` は、この特定のタスクを簡単にこなすためによく使われます。これは内部的に文字列を走査し、単語の区切りを認識した後に該当する文字を大文字に変換します。

実装の詳細では、大文字化の対象となる言語や地域特有の規則、Unicode文字の大文字/小文字の対応を考慮に入れる必要があります。Javaはこれらを `Locale` クラスを通じてサポートしており、多言語対応のアプリケーションに有益です。

また、パフォーマンスが懸念される場合は、`StringBuilder` や `StringBuffer` を使って大文字化のロジックを最適化できる場合があります。大規模な文字列操作を行う時には重要な観点です。

## See Also (関連情報)
- Java String Documentation: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- Apache Commons Lang - WordUtils: https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/text/WordUtils.html
- Stack Overflow discussion on string capitalization in Java: https://stackoverflow.com/questions/1892765/how-to-capitalize-the-first-character-of-each-word-in-a-string
