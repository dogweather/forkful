---
title:                "C: 「二つの日付の比較」"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ
日付の比較をする必要性は、日々の生活で数え方が異なる日付情報を扱う必要があるからです。

## 方法
下記のコード例を参考に、C言語で日付の比較をする方法を説明します。

```C
#include <stdio.h>

int main(void) {
    // 宣言と初期化
    int date1 = 20201115;
    int date2 = 20201116;
    // 日付の比較
    int comparison = (date1 > date2) - (date1 < date2);
    // 結果の出力
    printf("date1とdate2の比較結果: %d\n", comparison);
    return 0;
}
```

上記のコードを実行すると、`date1`と`date2`の比較結果が表示されます。`date1`が`date2`よりも大きい場合には1、小さい場合には-1、同じ場合には0が表示されます。

## 深堀り
日付の比較には、より詳細な方法もあります。例えば、`date1`と`date2`の差を求めることで日数の計算ができます。また、さまざまな日付フォーマットに対応する方法もあります。

## See Also
- [C言語の日付処理: 日付の取り扱い方から曜日の求め方まで (秋月秀一郎)](http://www.kumei.ne.jp/c_lang/c_time.htm)
- [C言語で日付や時間を扱う方法 (Nameless CJ)](https://qiita.com/NamelessCJ/items/4fc68e2e2718f34ec3c1)