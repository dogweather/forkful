---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:52.107756-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.566713-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3068\u3044\u3046\u306E\u306F\u3001\u6587\u5B57\u5217\u306E\u5F62\u5F0F\u3092\u89E3\
  \u91C8\u3057\u3066\u3001\u65E5\u3001\u6708\u3001\u5E74\u306E\u3088\u3046\u306A\u65E5\
  \u4ED8\u306E\u8981\u7D20\u3092\u62BD\u51FA\u3059\u308B\u3053\u3068\u3092\u610F\u5473\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30E6\u30FC\
  \u30B6\u30FC\u5165\u529B\u3092\u51E6\u7406\u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\
  \u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u53D6\u3063\u305F\u308A\u3001\u6587\u5B57\
  \u5217\u5F62\u5F0F\u3067\u65E5\u4ED8\u3092\u4F1D\u9054\u3059\u308BAPI\u3068\u5BFE\
  \u8A71\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u3067\u306E\u30C7\u30FC\u30BF\u51E6\u7406\u3001\u691C\u8A3C\u3001\u304A\u3088\
  \u3073\u65E5\u4ED8\u7B97\u8853\u3092\u5B9F\u884C\u3059\u308B\u305F\u3081\u306B\u4E0D\
  \u53EF\u6B20\u3067\u3059\u3002."
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
