---
title:                "Arduino: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

なぜ私たちは正規表現を使うのでしょうか？正規表現は、異なる形式のテキストデータから特定のパターンや文字列を抽出する必要があるときに非常に役立ちます。例えば、電話番号やメールアドレスのような特定の形式を持つデータを取得する場合に、正規表現を使用することで時間を節約し、簡単にデータを抽出することができます。さらに、正規表現はArduinoでのテキスト解析や文字列処理にも有用です。

## 使い方

正規表現を使うためには、まず「Arduino IDE」をインストールして開きます。次に、「ライブラリマネージャ」から「Regexp」ライブラリをインストールします。これにより、通常のArduinoプログラムと同様に、「#include <Regexp.h>」のように正規表現ライブラリをインポートすることができます。以下の例では、テキストデータから電話番号を抽出する方法を示します。

```arduino
#include <Regexp.h> //正規表現ライブラリをインポート

String text = "私の電話番号は080-1234-5678です"; //テキストデータの定義

//Regexpクラスのインスタンスを作成
Regexp pattern("\\d{3}-\\d{4}-\\d{4}"); //正規表現パターンを指定
MatchState ms; //マッチ状態を格納する変数を作成
ms.Target(text); //テキストデータをマッチング対象に設定

if (ms.Match(pattern)) {
  //マッチした部分を抽出して表示
  Serial.println(ms.GetCapture(0));
}
```

上記のコードを実行すると、シリアルモニタに「080-1234-5678」が表示されるはずです。

## ディープダイブ

さらに高度な正規表現の使い方を見てみましょう。正規表現には、様々なメタ文字が存在します。代表的なものとしては、任意の1文字を表す「.」や、0回以上の繰り返しを表す「*」があります。また、「|」を使うことで複数のパターンを指定することもできます。これらのメタ文字を組み合わせることで、さらに複雑なパターンのマッチングが可能になります。

例えば、以下のコードでは、テキストデータから「apple」や「orange」のような果物の名前を抽出しています。

```arduino
String text = "私はリンゴが好きです。しかし、オレンジも美味しいですね。";

Regexp pattern("リンゴ|オレンジ"); //「リンゴ」または「オレンジ」にマッチするパターンを指定

//マッチした部分を全て抽出して表示
while (ms.Match(pattern)) {
  Serial.println(ms.GetCapture(0));
}
```

上記のコードを実行すると、シリアルモニタに「リンゴ」、「オレンジ」が順番に表示されるはずです。

## See Also

- [正規表現入門](https://itsakura.com/regex)
- [正規表現チュートリアル](https://www.codementor.io/@ravianand1988