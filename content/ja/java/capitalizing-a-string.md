---
title:    "Java: 文字列を大文字化する"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字にするのに、なぜ誰かが参加するかの説明を、1〜2文で示す。

文字列を大文字にすることは、プログラミングにおいて非常に便利で、データ処理や出力での一貫性を保つのに役立ちます。

## 方法

```Java
// 文字列を大文字にするメソッドの例
public static String toUpperCase(String str) {
    StringBuilder result = new StringBuilder();
    for (int i = 0; i < str.length(); i++) {
        char c = str.charAt(i);
        if (Character.isLetter(c)) {
            result.append(Character.toUpperCase(c));
        } else {
            result.append(c);
        }
    }
    return result.toString();
}
```

**入力**："hello world"  
**出力**："HELLO WORLD"

この例では、まず与えられた文字列をループで走査し、文字がアルファベットの場合は大文字に変換し、それ以外の場合はそのまま出力するようにしています。

## 深く掘り下げる

文字列を大文字にするメソッドには、いくつかの実装方法があります。例えば、Javaの標準ライブラリには`toUpperCase()`というメソッドが用意されており、これを使用することでも文字列を大文字にできます。また、正規表現やStream APIを使用しても同様の結果を得ることができます。

また、多言語対応を考慮する必要がある場合や、特殊文字の扱いを決める必要がある場合には、さらに複雑な実装が必要になるかもしれません。

## もっと詳しく知りたい方へ

ここまでの例では、文字列を大文字に変換する際に全ての文字を変換していますが、実際には例外的な文字や言語によるルールも存在します。下記のリンクは、これらのルールをより詳しく説明した記事です。

[文字の大文字と小文字の変換のルール](https://techacademy.jp/magazine/23791)

[Javaの正規表現による文字列の変換方法](https://www.tomoare.com/column/columnJava/e-commerce/javaRegex2.html)

## 関連サイト

[JavaのStringクラスドキュメント](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)

[JavaのStream APIドキュメント](https://docs.oracle.com/javase/8/docs/api/java/util/stream/package-summary.html)

[Javaで文字列を操作する方法](https://techbookfest.org/product/5670822523514368)

---
## 関連するものを見る

[文字列を小文字に変換する方法](https://github.com/mimorin2017/markdown_blog/blob/master/how_to_lowercase_string.md)