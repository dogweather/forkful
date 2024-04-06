---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:52.107756-07:00
description: "\u65B9\u6CD5\uFF1A \u30E2\u30C0\u30F3\u306AC++\u3067\u306F\u3001 `<chrono>`\
  \ \u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u3066\u65E5\u4ED8\u3068\
  \u6642\u523B\u3092\u30CD\u30A4\u30C6\u30A3\u30D6\u306B\u6271\u3046\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u304C\u3001\u3088\u308A\u8907\u96D1\u306A\u5F62\u5F0F\u306E\
  \u624B\u52D5\u89E3\u6790\u306A\u3057\u306B\u6587\u5B57\u5217\u304B\u3089\u89E3\u6790\
  \u3059\u308B\u3053\u3068\u3092\u76F4\u63A5\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\
  \u307E\u305B\u3093\u3002\u305F\u3060\u3057\u3001ISO 8601\u65E5\u4ED8\u5F62\u5F0F\
  \u304A\u3088\u3073\u7C21\u5358\u306A\u30AB\u30B9\u30BF\u30E0\u5F62\u5F0F\u306E\u5834\
  \u5408\u3001\u89E3\u6790\u3092\u884C\u3046\u65B9\u6CD5\u306F\u6B21\u306E\u3068\u304A\
  \u308A\u3067\u3059\u3002\u2026"
lastmod: '2024-04-05T21:53:43.375659-06:00'
model: gpt-4-0125-preview
summary: "**`<chrono>` \u3068 `<sstream>` \u3092\u4F7F\u7528\u3059\u308B:**."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

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
