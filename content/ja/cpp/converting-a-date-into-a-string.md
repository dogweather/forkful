---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

日付を文字列に変換するとは、日付データを文字列形式に整形することを言います。これにより、プログラマは日付データを読み易い形式で表示し、また簡単に日付の比較やソートが可能になります。

## 実装方法:

以下に示すコードは、C++で日付を文字列に変換する一例です。これには、C++の `<chrono>` と `<iomanip>` ライブラリを使用します。

```C++
#include<iostream>
#include<chrono>
#include<iomanip>

int main() {
    // 今の時間を取得する
    auto now = std::chrono::system_clock::now();
    std::time_t now_t = std::chrono::system_clock::to_time_t(now);
    // 時間を文字列に変換する
    std::cout << std::put_time(std::localtime(&now_t), "%Y-%m-%d %X") << std::endl;
    return 0;
}
```

これを実行すると、現在の日時が YYYY-MM-DD HH:MM:SS 形式で出力されます。

## ディープダイブ:

日付を文字列に変換する方法は昔から様々ありました。古くは `sprintf` や `strftime` といった関数を使っていましたが、セキュリティ上の問題や使いづらさから新しいライブラリが次々と提供されてきました。

C++には `<chrono>` や `<iomanip>` といった標準的なライブラリがありますが、より高機能な日付処理が必要な場合には `boost::date_time` などの外部ライブラリを利用することも可能です。

また、日付を文字列に変換する際のフォーマット指定も重要です。ここでは C や Unix の系譜を引く `%Y-%m-%d %X` 形式を使用しましたが、需要に応じて様々な形式を選ぶことができます。

## 参考記事:

- [C++ `<chrono>` ライブラリについて学ぶ](https://en.cppreference.com/w/cpp/chrono)
- [`put_time` と `<iomanip>` ライブラリについて理解する](https://www.cplusplus.com/reference/iomanip/put_time/)
- [Unix 時間フォーマットについて深く学ぶ](https://www.unix.com/man-page/linux/3/strftime/)
- [`boost::date_time` ライブラリの詳細](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)