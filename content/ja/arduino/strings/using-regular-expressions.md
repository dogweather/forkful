---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:10.544853-07:00
description: "\u65B9\u6CD5 Arduino\u306F\u3001\u305D\u306E\u6A19\u6E96\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u5185\u3067\u76F4\u63A5regex\u3092\u30B5\u30DD\u30FC\u30C8\u3057\
  \u3066\u3044\u307E\u305B\u3093\u3002\u3057\u304B\u3057\u3001\u57FA\u672C\u7684\u306A\
  \u6587\u5B57\u5217\u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u5358\u7D14\u306A\u30D1\
  \u30BF\u30FC\u30F3\u306B\u5BFE\u3059\u308Bregex\u306E\u3088\u3046\u306A\u6A5F\u80FD\
  \u3092\u5B9F\u73FE\u3057\u305F\u308A\u3001\u3088\u308A\u8907\u96D1\u306A\u30CB\u30FC\
  \u30BA\u306B\u5BFE\u3057\u3066\u306F`regex`\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\
  \u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u7D71\u5408\u3059\u308B\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002 #."
lastmod: '2024-03-13T22:44:42.481941-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u306F\u3001\u305D\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u5185\u3067\u76F4\u63A5regex\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\
  \u305B\u3093\u3002\u3057\u304B\u3057\u3001\u57FA\u672C\u7684\u306A\u6587\u5B57\u5217\
  \u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u5358\u7D14\u306A\u30D1\u30BF\u30FC\u30F3\
  \u306B\u5BFE\u3059\u308Bregex\u306E\u3088\u3046\u306A\u6A5F\u80FD\u3092\u5B9F\u73FE\
  \u3057\u305F\u308A\u3001\u3088\u308A\u8907\u96D1\u306A\u30CB\u30FC\u30BA\u306B\u5BFE\
  \u3057\u3066\u306F`regex`\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\
  \u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u7D71\u5408\u3059\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 方法
Arduinoは、その標準ライブラリ内で直接regexをサポートしていません。しかし、基本的な文字列関数を使用して単純なパターンに対するregexのような機能を実現したり、より複雑なニーズに対しては`regex`のようなサードパーティライブラリを統合することができます。

### Regexを使わない基本的な文字列マッチング
部分文字列を見つけるような基本的なニーズのためには、`String.indexOf()`関数を使用することができます:
```cpp
String data = "Sensor value: 12345";
int index = data.indexOf("value:");
if (index != -1) {
  String value = data.substring(index + 6).trim();
  Serial.println(value); // 出力: 12345
}
```

### Regex用のサードパーティライブラリの利用
より複雑なパターンを扱うためには、`regex`のようなライブラリを検討することができます。ライブラリをインストールした後、以下のように使用することができます:

1. **インストール**: `regex`ライブラリは、Arduinoライブラリマネージャーで直接利用可能でない場合があるため、信頼できるソースからダウンロードして、Arduinoのライブラリフォルダに追加することで手動でインストールする必要があるかもしれません。

2. **例示的な使用方法**:
ライブラリが標準のregex実装に似た機能を提供していると仮定すると、以下のように使用することができます:

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // Serialが準備できるのを待つ
  
  regex_t reg;
  const char* pattern = "[0-9]+"; // 数字の連続にマッチ
  regcomp(&reg, pattern, REG_EXTENDED);
  
  const char* test_str = "Sensor value: 12345";
  
  regmatch_t matches[1];
  if (regexec(&reg, test_str, 1, matches, 0) == 0) {
    // マッチした部分を抽出して出力
    int start = matches[0].rm_so;
    int end = matches[0].rm_eo;
    char match[end-start+1];
    strncpy(match, test_str + start, end-start);
    match[end-start] = '\0';
    
    Serial.print("マッチを検出: ");
    Serial.println(match); // 出力: 12345
  } else {
    Serial.println("マッチは見つかりませんでした");
  }
  
  regfree(&reg); // regex用に割り当てられたメモリを解放
}

void loop() {
  // 主なコードをここに置く、繰り返し実行される:
}
```

**注意**: ここで使用されている構文や特定の関数は説明の目的であり、選択する`regex`ライブラリの実際の実装の詳細に基づいて変わる可能性があります。正確で最新の情報については、必ずライブラリのドキュメントを参照してください。
