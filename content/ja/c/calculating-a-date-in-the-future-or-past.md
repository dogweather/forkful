---
title:                "未来や過去の日付の計算"
html_title:           "C: 未来や過去の日付の計算"
simple_title:         "未来や過去の日付の計算"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# なぜ？
人々が未来や過去の日付を計算するには、様々な理由があります。誕生日や記念日の計算、締め切りや期限の確認、あるいは単に予定を立てるための準備として、日付の計算は非常に役立つものです。

## やり方
日付を計算するには、C言語でいくつかの基本的なコードを覚える必要があります。まずは、日付を表す「年」「月」「日」の変数を用意し、その日付に対して計算したい日数を入力します。次に、標準のライブラリ関数である「mktime」を使って日付を計算し、結果を表示します。以下のコードを参考にしてみてください。

```C
#include <stdio.h>
#include <time.h>

int main() {
    // 日付の変数を定義
    int year = 2020;
    int month = 10;
    int day = 10;
    
    // 何日後の日付を計算するか入力
    int days = 30;
    
    // mktimeで日付を計算
    struct tm date = {0};
    date.tm_year = year - 1900; //tm_yearは1900年からの経過年数を表すため、入力された年から1900を引いておく必要がある
    date.tm_mon = month - 1; //tm_monは0からの月を表すため、入力された月から1を引いておく必要がある
    date.tm_mday = day;
    
    time_t new_date = mktime(&date);
    new_date += days * 86400; //1日は86400秒で表されるため、daysに86400を掛ける
    struct tm *result = localtime(&new_date);
    
    // 結果を表示
    printf("日付の計算結果は %d年%d月%d日 です。\n", result->tm_year + 1900, result->tm_mon + 1, result->tm_mday);
    
    return 0;
}
```

上記のコードを実行すると、2020年10月10日から30日後の日付が計算されて表示されます。

```
日付の計算結果は 2020年11月9日 です。
```

## ディープダイブ
深く日付を計算するためには、C言語の日付に関する標準ライブラリ関数「time.h」を詳しく学ぶ必要があります。このライブラリには、日付や時間の情報を取得したり、日付の計算や比較を行ったりするための便利な関数が多数含まれています。また、UNIX時間やレピュニット（1秒間に1億回の動作をするタイマー）など、日付に関する特殊な概念も学ぶことができます。

# 関連リンク
- [mktime関数の仕様（公式ドキュメント）](https://en.cppreference.com/w/c/chrono/mktime)
- [time.hライブラリのまとめ（Qiita）](https://qiita.com/yohhoy/items/5411eac2a4c325c57522)
- [UNIX時間とは？（Techacademy）](https://techacademy.jp/magazine/8641)
- [レピュニットとは？（Wikipedia）](https://ja.wikipedia.org/wiki/%E3%83%AC%E3%83%94%E3%83%A5%E3%83%8B%E3%83%83%E3%83%88)