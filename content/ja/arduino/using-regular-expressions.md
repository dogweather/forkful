---
title:    "Arduino: 『正規表現を使用する』"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## なぜ正規表現を使うのか

正規表現は、テキストデータから特定のパターンを検索したり、置き換えたりするための非常に便利なツールです。Arduinoプログラミングでは、ユーザーからの入力を受け取ったり、センサーデータを処理したりする際に正規表現を使用することができます。

## 使い方

正規表現を使用するには、まずArduinoのライブラリである"Regex"をインポートする必要があります。次に、Regexオブジェクトを作成し、そのオブジェクトを使用して文書内のパターンを検索または置き換えすることができます。

```
#include <Regex.h>

Regex myRegex("[0-9]+");
// 文書内の数字のパターンを検索する

String myString = "今日の気温は25度です。";
if (Regex.match(myString)) {
  Serial.println("気温を検出しました！");
}
```

上記の例では、正規表現を使用して文書内の数字のパターンを検索しています。もちろん、他のパターンを検索したり、置き換えたりすることもできます。

## もっと詳しく

正規表現についてもっと詳しく知りたい場合は、以下のリンクをご参照ください。

- [正規表現チュートリアル](https://www.programmingsimplified.com/javascript/regular-expression-tutorial)
- [正規表現の使用例](https://www.oreilly.com/library/view/regular-expressions-cookbook/9781449327453/ch04s05.html)

## 参考リンク

- [ArduinoのRegexライブラリドキュメント](https://www.arduino.cc/reference/en/libraries/regexp/)
- [正規表現の基礎](https://techacademy.jp/magazine/107152)
- [正規表現入門](https://www.tohoho-web.com/ex/reg/regex.html)

---

## 関連記事

- [Arduinoで使用する便利なライブラリまとめ](https://qiita.com/naoyabigaoka/items/4c784dad2156b74f2560)