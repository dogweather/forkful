---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何と、なぜ? (What & Why?)
部分文字列を抽出するとは文字列から特定の部分を取り出すことです。「こんにちは、世界」から「世界」を抽出したり、日時の特定の要素（例：年）を取り出したりできます。プログラマは複雑な文字列を扱いやすくするため、また特定のデータを視覚化や操作するためにこれを行います。

## ハウツー (How to:)
Javaで部分文字列を抽出する方法をみてみましょう。次に示すいくつかの例を参考にしてください。

```Java
public class SubstringExample {
    public static void main(String[] args) {
        String sentence = "こんにちは、世界";
        String extracted = sentence.substring(5, 7);
        System.out.println(extracted);  // Output: 世界
    }
}
```
この例では、`substring`メソッドを使って、文字列から「世界」を抽出しました。このメソッドは指定された範囲の文字を新しい文字列として返します。

```Java
public class SubstringExample {
    public static void main(String[] args) {
        String dateTime = "2022-01-31 10:30:00";
        String date = dateTime.substring(0, 10);
        System.out.println(date);  // Output: 2022-01-31
    }
}
```
この例では、日付と時刻が格納された文字列から日付部分のみを抽出しています。

## ディープダイブ (Deep Dive)
部分文字列を抽出する方法はJavaが発展するにつれて進化しました。初期のバージョンでは`substring`メソッドは空間がかなり可能性のある場合でも元の文字列の全体を保持してしまいました。これは、メモリ上で冗長なデータを保持するという問題を引き起こしました。現在のJavaバージョンでは、よりメモリ効率のよい方式に更新して対応されています。

代替手段として、`split`メソッドや`charAt`メソッド、あるいはJava 8以降の`streams`や`lambda`を利用することもできます。しかし最善の方法は、問題の具体的な状況と要件によります。

もう一つ、`substring`メソッドは、範囲が無効な場合(文字列の長さを超える場合)には`StringIndexOutOfBoundsException`をスローしますのでご注意ください。

## 参照先 (See Also)
次のリンクは部分文字列の抽出、Javaの文字列操作、及びそれらの機能を詳しく説明しています。

- [Oracle's Java Documentation on `substring`](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html#substring(int,int))
- [Baeldung's Guide on Java Substrings](https://www.baeldung.com/java-substring)
- [Java String split() with examples](https://www.javatpoint.com/java-string-split)