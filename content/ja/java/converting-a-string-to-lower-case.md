---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なんで？どうして？
文字列を小文字へ変換（toLowerCase()）するとは、すべての文字列を小文字の形に変えることです。コードの一貫性とデータ比較の正確性を保ちためによく行われます。

## どうやって：
以下がJavaの簡単なコーディング例です：

```Java
public class Main {
    public static void main(String[] args) {
        String myStr = "Hello, World!";
        String lowerCaseStr = myStr.toLowerCase();
        System.out.println(lowerCaseStr);
    }
}
```

実行結果：

```Java
hello, world!
```

## ディープダイブ
歴史的な文脈：toLowerCase()はJavaで初の文字列操作関数の一つで、多くの言語に影響を与えました。

代替手段：大文字と小文字を無視した比較を行う場合、equalsIgnoreCase()関数を使用できます。

実装の詳細：toLowerCase()メソッドは、Unicodeに基づいて文字列内の各文字をその小文字版に変換します。特殊なケースとして、いくつかの言語（トルコ語など）では特定の文字への変換が異なる可能性があります。

## 参考リンク
Java String toLowerCase()メソッドの詳細なドキュメンテーションを下記のリンクから参照できます：

- [Oracle Java Documentation](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#toLowerCase())
- [Tutorialspoint - Java toLowerCase](https://www.tutorialspoint.com/java/java_string_tolowercase.htm)

また、toUpperCase()関数についても同様に理解することをお勧めします。以下のリンクを参照ください：

- [Oracle Java Documentation](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#toUpperCase())
- [Tutorialspoint - Java toUpperCase](https://www.tutorialspoint.com/java/java_string_touppercase.htm)