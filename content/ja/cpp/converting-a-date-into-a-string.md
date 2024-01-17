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

## 何が & なぜ?
日付を文字列に変換することは、プログラマーが日付をより扱いやすい形式にするための方法です。プログラマーは、日付を処理する必要があるさまざまなプログラムやアプリケーションにおいて、日付を簡単に操作できるようにするためにこの方法を使用します。

## 方法:
```C++ 
#include <iostream>
#include <ctime>
#include <sstream>

using namespace std;

// 日付を文字列に変換する関数
string dateToString(time_t date) {
  // 日付を文字列に変換するstream
  ostringstream s;
  // フォーマットを指定
  s << put_time(localtime(&date), "%m/%d/%Y");
  // 変換した文字列を返す
  return s.str();
}

int main() {
  // 現在の日付を取得
  time_t date = time(0);
  // 変換した文字列を出力
  cout << dateToString(date) << endl;
  return 0;
}
```

出力: ```09/28/2021```


## 詳細を調べる:
日付を文字列に変換する方法は、古くからプログラミング言語に存在します。しかし、どのように実装されているかは言語によって異なります。C++においては、```<ctime>```ヘッダーファイルと```time_t```データ型を使用し、日付を操作することができます。また、日付を他の形式に変換する方法もあります。例えば、```std::chrono```ライブラリを使用することで、時刻単位の変換ができます。

## 関連リンク:
- [C++ Reference](https://www.cplusplus.com/reference/)
- [std::chrono ライブラリ](https://www.cplusplus.com/reference/chrono/)
- [時間と日付の操作について学ぼう](https://www.tohoho-web.com/ex/c.htm#chrono veryhighlevel)