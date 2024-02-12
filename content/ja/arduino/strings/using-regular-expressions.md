---
title:                "正規表現の使用"
date:                  2024-02-03T19:16:10.544853-07:00
model:                 gpt-4-0125-preview
simple_title:         "正規表現の使用"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
正規表現（regex）は、文字列のマッチングや操作に主に使用される、検索パターンを定義する文字のシーケンスです。プログラマーは、シリアル入力の解析、ユーザー入力の検証、文字列からのデータの抽出など、Arduinoプロジェクトでregexを活用し、データ処理の効率と柔軟性を向上させます。

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