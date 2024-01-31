---
title:                "正規表現の使用"
date:                  2024-01-19
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)

正規表現とは文字列パターンを識別・操作するためのテキスト式。プログラマは煩雑なテキスト処理を効率化するために使用する。

## How to (実装方法):

```Arduino
#include <regex.h>

void setup() {
  Serial.begin(9600);
  // 待機状態になるまで待つ
  while (!Serial) {}
  
  regex_t reg;
  const char * pattern = "[A-Za-z]+"; // 英字のみを検出するパターン
  char inputString[] = "Arduino123Programming456";

  if(regcomp(&reg, pattern, REG_EXTENDED) == 0) { // 正規表現をコンパイル
    regmatch_t matches[10]; // マッチ情報を格納する配列
    if(regexec(&reg, inputString, 10, matches, 0) == 0) { // マッチを実行
      for(int i = 0; i < 10 && matches[i].rm_so != -1; i++) {
        int start = matches[i].rm_so;
        int end = matches[i].rm_eo;
        Serial.print("Match: ");
        Serial.println(String(inputString).substring(start, end));
      }
    } else {
      Serial.println("No match found");
    }
  } else {
    Serial.println("Regex pattern compilation failed");
  }
  regfree(&reg); // リソースの解放
}

void loop() {
  // ここでは何もしません。
}
```

サンプル出力:

```
Match: Arduino
Match: Programming
```

## Deep Dive (深堀り):

正規表現は1940年代の数学概念。多言語ライブラリとの互換性を求めるなら`std::regex`の代わりに`regex.h`をアルドゥイーノで使用する。しかし、正規表現を使わずに文字列関数（`indexOf`, `substring`など）で簡単な文字列操作可能。

## See Also (関連リンク):

- 正規表現メタキャラクタ一覧: https://www.boost.org/doc/libs/release/libs/regex/doc/html/boost_regex/syntax/perl_syntax.html
- Arduino `String` クラス: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- `regex.h` マニュアル: https://linux.die.net/man/3/regex
