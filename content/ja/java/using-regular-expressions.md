---
title:                "正規表現の使用"
html_title:           "Java: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使用する理由はたくさんありますが、一番の理由はデータのパターンを効率的にマッチングできることです。これにより、大量のデータを手動で処理する必要がなくなり、時間の節約につながります。

## 使い方

正規表現を使用するには、まず ```java java.util.regex``` パッケージをインポートします。次に、パターンを定義し、マッチしたい文字列と比較します。

```java
import java.util.regex.*;

String str = "今日の天気は晴れです";
String pattern = ".*晴れ.*"; // マッチしたいパターンを定義する
Pattern p = Pattern.compile(pattern);
Matcher m = p.matcher(str);
if (m.find()) { // マッチしたらtrueを返す
    System.out.println("天気が晴れです！");
} else {
    System.out.println("天気が晴れではありません。");
}
```

上記のコードでは、パターン ```.*晴れ.*``` が文字列にマッチしているかどうかを確認しています。 ```.*``` は任意の文字列を表し、その前後に ```晴れ``` という文字列があるかどうかをチェックします。もしマッチしていれば、```天気が晴れです！``` というメッセージが出力されます。

複雑なパターンを定義することも可能です。例えば、電話番号の形式をチェックする場合、```[0-9]{2,4}-[0-9]{3,4}-[0-9]{4}``` というパターンを定義し、それにマッチするかどうかを確認します。詳しくは「深堀」セクションで説明します。

## 深堀

正規表現はテキスト内の指定したパターンを検索するための特別な文字列です。基本的には、任意の文字を表す「ワイルドカード」や特定の文字を指定する「キャラクタークラス」を使用して、パターンを定義します。例えば、```.*``` は任意の文字列を表し、```[a-zA-Z]``` はアルファベットの小文字と大文字を指定します。詳しい正規表現のルールについては、以下のリンクを参考にしてください。

- [正規表現チュートリアル](https://www.ntu.edu.sg/home/ehchua/programming/howto/Regexe.html)
- [Javaの正規表現について](https://docs.oracle.com/javase/tutorial/essential/regex/index.html)

## 関連リンク

- [Java正規表現リファレンス (Oracle)](https://docs.oracle.com/javase/jp/6/api/java/util/regex/Pattern.html)
- [正規表現を使用したテキスト処理 (JavaMagazine)](https://www.javamagazine.mozaicreader.com/JulAug2018/Launch.action#article-id=63425)
- [練習用の正規表現サイト (Regex101)](https://regex101.com/)