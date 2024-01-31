---
title:                "JSONを扱う方法"
date:                  2024-01-19
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (なにを？どうして？)

JSONはデータ交換のフォーマット。軽量で読み書きが簡単。プログラマはウェブAPIと通信したり設定を保存したりするために使う。

## How to: (やり方)

C言語でのJSON処理にはライブラリが必要。`jansson`など。次は`jansson`を使った例。

```C
#include <jansson.h>
#include <stdio.h>

int main() {
    // JSONオブジェクトを作成
    json_t *object = json_object();
    json_object_set_new(object, "name", json_string("Taro"));
    json_object_set_new(object, "age", json_integer(25));
    
    // JSON文字列に変換
    char *json_string = json_dumps(object, JSON_INDENT(2));
    printf("%s\n", json_string);
    
    // メモリ解放
    free(json_string);
    json_decref(object);
    
    return 0;
}
```

出力:
```
{
  "name": "Taro",
  "age": 25
}
```

## Deep Dive (掘り下げ)

JSONはJavaScriptのサブセットとして1999年に生まれた。バイナリ形式のBSONやXMLと比べて軽量。C言語での実装は標準ライブラリにはないため、`cJSON`や`Jansson`, `json-c`などのサードパーティライブラリを使う。

## See Also (関連情報)

- Janssonライブラリ: http://www.digip.org/jansson/
- cJSONライブラリ: https://github.com/DaveGamble/cJSON
- JSON公式ウェブサイト: https://www.json.org/json-en.html
