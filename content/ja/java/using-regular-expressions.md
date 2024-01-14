---
title:                "Java: 正規表現を使う"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

# なぜ正規表現を使うのか

正規表現は、文字列のあらゆる部分を検索、抽出、置換するための強力なツールです。Javaプログラミングにおいては、特にテキスト処理やフォーマットチェックなどの場面で重要な役割を果たします。慣れるまでは難しく感じるかもしれませんが、使い方を学ぶことで生産性を大幅に向上させることができます。

# 正規表現の使い方

正規表現を使うには、まず java.util.regexパッケージをインポートする必要があります。コンパイルしたパターンを利用して、文字列を検索・置換する方法を見てみましょう。

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {
    public static void main(String[] args) {
        String text = "今日は晴れでしょう。明日は曇り。";

        Pattern pattern = Pattern.compile("今日|明日");
        Matcher matcher = pattern.matcher(text);

        while (matcher.find()) {
            System.out.println("マッチングした文字列: " + matcher.group());
        }
    }
}
```

上記のコードを実行すると、"今日"と"明日"の2つの文字列がマッチングした結果が出力されます。このように、パターンを定義してマッチングさせることで、特定の文字列を簡単に抽出することができます。

# 正規表現の深い理解

正規表現を使いこなすためには、パターンの文法やメタキャラクターなどの基本的な概念を理解する必要があります。また、グループ化やキャプチャ、バックリファレンスといったより高度な機能を使いこなせるようになると、より複雑なパターンを扱うことができるようになります。正規表現は強力なツールですが、誤ったパターンを定義すると予期せぬ結果が出力されることもあるので注意が必要です。

# また見る

- [Java正規表現チュートリアル](https://www.javatpoint.com/java-regex)
- [正規表現クイックリファレンス](https://www.javatpoint.com/java-regex-quick-reference)
- [正規表現エディター](https://regex101.com/)