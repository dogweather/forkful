---
title:                "文字列から日付をパースする"
aliases:
- ja/cpp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:52.107756-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から日付をパースする"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
文字列から日付を解析するというのは、文字列の形式を解釈して、日、月、年のような日付の要素を抽出することを意味します。プログラマーは、ユーザー入力を処理したり、データファイルを読み取ったり、文字列形式で日付を伝達するAPIと対話したりするためにこれを行います。これは、アプリケーションでのデータ処理、検証、および日付算術を実行するために不可欠です。

## 方法：
モダンなC++では、 `<chrono>` ライブラリを使用して日付と時刻をネイティブに扱うことができますが、より複雑な形式の手動解析なしに文字列から解析することを直接サポートしていません。ただし、ISO 8601日付形式および簡単なカスタム形式の場合、解析を行う方法は次のとおりです。

**`<chrono>` と `<sstream>` を使用する:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // ISO 8601 形式
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "解析された日付: " << parsed_date << std::endl;
    } else {
        std::cout << "日付の解析に失敗しました。" << std::endl;
    }
    
    return 0;
}
```
サンプル出力:
```
解析された日付: 2023-04-15
```

より複雑な形式を扱う場合や、古いC++バージョンを使っている場合には、`date.h`(Howard Hinnant の日付ライブラリ) のようなサードパーティのライブラリが人気です。それを使ってさまざまな形式を解析する方法は次のとおりです：

**`date.h`ライブラリを使用する:**
ライブラリがインストールされていることを確認してください。[こちら](https://github.com/HowardHinnant/date)で見つけることができます。

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "April 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "解析された日付: " << parsed_date << std::endl;
    } else {
        std::cout << "文字列からの日付の解析に失敗しました。" << std::endl;
    }

    return 0;
}
```
サンプル出力（システムのロケールと日付設定に応じて異なる場合があります）:
```
解析された日付: 2023-04-15
```
