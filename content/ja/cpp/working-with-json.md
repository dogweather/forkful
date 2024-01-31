---
title:                "JSONを扱う方法"
date:                  2024-01-19
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"

category:             "C++"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？とは？)
JSON (JavaScript Object Notation)はデータ交換用のテキストフォーマットです。シンプルで軽量なため、APIからデータを受け取ったり、設定情報を保存する際によく使われます。

## How to: (方法)
C++では、nlohmann/json ライブラリが使いやすいです。以下は基本的な使い方です。

```C++
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // JSON 文字列を作る
    nlohmann::json j = {
        {"name", "Tanaka"},
        {"age", 30},
        {"is_programmer", true}
    };
    
    // JSON を文字列に変換する
    std::string j_string = j.dump();
    std::cout << j_string << std::endl;
    
    // 文字列から JSON オブジェクトを作る
    auto j_parsed = nlohmann::json::parse(j_string);
    std::cout << "Name: " << j_parsed["name"] << ", Age: " << j_parsed["age"] << std::endl;
    
    return 0;
}
```

出力:
```
{"age":30,"is_programmer":true,"name":"Tanaka"}
Name: Tanaka, Age: 30
```

## Deep Dive (深掘り)
JSONは2001年にDouglas Crockfordによって考案されました。XMLより軽量で読み書きが簡単なことから、Webアプリケーションで好まれています。代替としてYAMLやTOMLがありますが、JSONは実装が楽で汎用性が高いです。C++でJSONを扱う際は、nlohmann/jsonやRapidJSONなどのライブラリがあります。nlohmann/jsonは扱いやすさで人気があり、RapidJSONはパフォーマンスに優れています。

## See Also (関連項目)
- nlohmann/json GitHubページ: https://github.com/nlohmann/json
- JSONの仕様: https://www.json.org/json-ja.html
- RapidJSON GitHubページ: https://github.com/Tencent/rapidjson
- JSONとXMLの比較: https://www.w3schools.com/js/js_json_xml.asp
