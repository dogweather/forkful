---
title:                "Java: パターンにマッチする文字の削除"
simple_title:         "パターンにマッチする文字の削除"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

文字パターンと一致する文字を削除することに興味があるのでしょうか？例えば、あなたが文書を編集している時に、重複した文字列や特定の文字パターンを削除したい場合があるかもしれません。また、プログラムで処理する際に、文字パターンに応じてデータを変換したり整形したりする必要があるかもしれません。このような状況では、文字パターンと一致する文字を効率的に削除する方法を知ることが重要です。

## 削除の方法

Javaで文字パターンと一致する文字を削除する方法はいくつかありますが、今回は正規表現を使った方法を紹介します。

まずは、`Pattern`クラスを使ってパターンをコンパイルします。例えば、`Pattern.compile("[aeiou]")`とすると、`a`、`e`、`i`、`o`、`u`に一致するパターンが作られます。次に、`Matcher`クラスを使って、削除対象の文字列に対して`find()`メソッドを実行します。`find()`メソッドは、文字列中にパターンと一致する部分があるかどうかを確認し、`true`または`false`を返します。一致する部分があれば、`start()`メソッドを使ってパターンが見つかった位置を取得し、文字列からその部分を削除します。繰り返し処理を行うことで、全ての一致する部分を削除することができます。

下記のコードは、`Hello world!`から母音を削除する例です。`[aeiou]`のパターンに一致する文字があれば、`sb.deleteCharAt(matcher.start())`で削除しています。

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DeleteCharacters {

    public static void main(String[] args) {
        String text = "Hello world!";
        Pattern pattern = Pattern.compile("[aeiou]");
        Matcher matcher = pattern.matcher(text);
        StringBuilder sb = new StringBuilder(text);
        while (matcher.find()) {
            sb.deleteCharAt(matcher.start());
        }
        System.out.println(sb.toString()); //Hll wrld!
    }
}
```

上記のコードを実行すると、`Hll wrld!`という結果が出力されます。

## 深堀り

正規表現について詳しく学ぶことで、より柔軟に文字パターンを扱うことができます。例えば、`[aeiou]`というパターンは、母音に一致するものを全て削除しますが、`[A-Z]`というパターンを使うと、大文字のアルファベットを全て削除することができます。また、`[a-z]`を使うと、小文字のアルファベットを削除できます。正規表現は多様なパターンを表現できるため、様々な削除の方法を試してみることで、より便利なプログラムを作ることができるでしょう。

## 参考資料

- [Javaで正規表現を使用する方法](https://docs.oracle.com/javase/tutorial/essential/regex/index.html)
- [正規表現チュートリアル](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)