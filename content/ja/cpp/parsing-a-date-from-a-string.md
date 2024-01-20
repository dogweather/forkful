---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# 文字列から日付をパースする方法の解説：

## 何となぜ？(What & Why?)

文字列から日付のパースは、文字列形式の日付を計算可能な日付形式に変換するプロセスです。これは、日付が文字列として保存または送信され、後でそれを操る必要が生じるときにプログラマーにとって重要です。

## どのように？(How to?)

以下に示す通り、C++の`std::get_time`関数を使って文字列から日付を解析することができます:

```C++

#include <iostream>
#include <iomanip>
#include <sstream>
#include <ctime>

int main() {
    std::tm tm = {}; 
    std::string s("2022-09-09 20:30:00");
    std::istringstream ss(s);

    ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");
    if (ss.fail()) {
        std::cout << "Parse failed\n";
    } else {
        std::cout << std::put_time(&tm, "%c") << '\n';
    }
    return 0;
}

```

上記のコードが出力する結果は次のとおりです:

```shell
Fri Sep  9 20:30:00 2022
```

## ディープダイブ(Deep Dive)

1. 歴史的背景：文字列からの日付解析は、ユーザー入力やファイル、ウェブリクエストなどから受け取った日付情報を解析と操作する為に開発されました。
2. 代替手段：C++には、Boostなどのサードパーティライブラリを使って日付を解析するオプションもあります。
3. 実装の詳細：`std::get_time` は文字列を解析し、tm構造体へ結果を保存します。解析が失敗した場合、`failbit`が設定されます。

## 参照 (See Also) 

以下に、関連するリンクと情報を提供します:

1. [cplusplus.com](http://www.cplusplus.com/reference/ctime/get_time/) - `std::get_time`の詳細と使用方法について説明しています。
2. [Cppreference](https://en.cppreference.com/w/cpp/io/manip/get_time) - もっと深い理解のためのRvalue, Lvalueなどの概念と使い方を説明しています。
3. [Boost](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html) - C++のBoostライブラリの日付と時間操作についての詳細なドキュメンテーションがあります。