---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:19.097332-07:00
description: "\u65B9\u6CD5\uFF1A Fish Shell\u3067\u306F\u3001\u6587\u5B57\u5217\u304B\
  \u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\u305F\u3081\u306B\u7279\u5225\u306B\
  \u8A2D\u8A08\u3055\u308C\u305F\u7D44\u307F\u8FBC\u307F\u30B3\u30DE\u30F3\u30C9\u306F\
  \u3042\u308A\u307E\u305B\u3093\u3002\u4EE3\u308F\u308A\u306B\u3001`date`\uFF08Linux\u304A\
  \u3088\u3073macOS\u3067\u5229\u7528\u53EF\u80FD\uFF09\u306E\u3088\u3046\u306A\u5916\
  \u90E8\u30E6\u30FC\u30C6\u30A3\u30EA\u30C6\u30A3\u306B\u4F9D\u5B58\u3059\u308B\u304B\
  \u3001\u3088\u308A\u8907\u96D1\u306A\u89E3\u6790\u306B\u5411\u3044\u3066\u3044\u308B\
  `GNU\u2026"
lastmod: '2024-03-13T22:44:42.750423-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell\u3067\u306F\u3001\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\
  \u89E3\u6790\u3059\u308B\u305F\u3081\u306B\u7279\u5225\u306B\u8A2D\u8A08\u3055\u308C\
  \u305F\u7D44\u307F\u8FBC\u307F\u30B3\u30DE\u30F3\u30C9\u306F\u3042\u308A\u307E\u305B\
  \u3093\u3002\u4EE3\u308F\u308A\u306B\u3001`date`\uFF08Linux\u304A\u3088\u3073macOS\u3067\
  \u5229\u7528\u53EF\u80FD\uFF09\u306E\u3088\u3046\u306A\u5916\u90E8\u30E6\u30FC\u30C6\
  \u30A3\u30EA\u30C6\u30A3\u306B\u4F9D\u5B58\u3059\u308B\u304B\u3001\u3088\u308A\u8907\
  \u96D1\u306A\u89E3\u6790\u306B\u5411\u3044\u3066\u3044\u308B`GNU date`\u306E\u3088\
  \u3046\u306A\u4EBA\u6C17\u306E\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30C4\u30FC\
  \u30EB\u3092\u6D3B\u7528\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u304C\u305D\u306E\u30A2\
  \u30D7\u30ED\u30FC\u30C1\u3067\u3059\uFF1A\n\n**Fish\u3067`date`\u3092\u4F7F\u7528\
  \u3059\u308B\uFF1A**\n\n\"YYYY-MM-DD\"\u306E\u5F62\u5F0F\u306E\u65E5\u4ED8\u6587\
  \u5B57\u5217\u3092\u89E3\u6790\u3059\u308B\u306B\u306F\u3001`date`\u30B3\u30DE\u30F3\
  \u30C9\u3092`-d`\uFF08\u307E\u305F\u306FGNU date\u3067\u306F`--date`\uFF09\u30AA\
  \u30D7\u30B7\u30E7\u30F3\u306B\u7D9A\u3051\u3066\u6587\u5B57\u5217\u3092\u6307\u5B9A\
  \u3057\u3066\u4F7F\u7528\u3057\u307E\u3059\u3002\u51FA\u529B\u5F62\u5F0F\u3092\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u3059\u308B\u306B\u306F`+`\u30AA\u30D7\u30B7\u30E7\
  \u30F3\u3092\u4F7F\u7528\u3057\u307E\u3059."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

## 方法：
Fish Shellでは、文字列から日付を解析するために特別に設計された組み込みコマンドはありません。代わりに、`date`（LinuxおよびmacOSで利用可能）のような外部ユーティリティに依存するか、より複雑な解析に向いている`GNU date`のような人気のサードパーティツールを活用します。以下がそのアプローチです：

**Fishで`date`を使用する：**

"YYYY-MM-DD"の形式の日付文字列を解析するには、`date`コマンドを`-d`（またはGNU dateでは`--date`）オプションに続けて文字列を指定して使用します。出力形式をフォーマットするには`+`オプションを使用します。

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# 出力: Saturday, 01 April 2023
```

macOS用（`-j`と`-f`フラグに異なる形式が必要です）：

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# 出力: Saturday, 01 April 2023
```

**複雑な解析のためのGNU `date`の使用：**

GNU `date`は文字列形式に対してより柔軟です。入力形式を明示的に指定せずに、多くの一般的な日付文字列形式を自動的に検出することができます：

```fish
set complex_date_str "April 1, 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# 出力: 2023-04-01 14:00:00
```

ただし、自動的に認識されないかもしれない日付文字列を扱う場合や、入力形式について厳密な制御が必要な場合には、GNU `date`で入力形式を指定することは直接サポートされていません。そのような場合は、文字列を事前処理するか、より複雑な日付解析ルーティンのために設計された別のツールを使用することを検討してください。
