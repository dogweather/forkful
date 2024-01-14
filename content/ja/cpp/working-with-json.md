---
title:                "C++: 「JSONの扱い方」"
simple_title:         "「JSONの扱い方」"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/working-with-json.md"
---

{{< edit_this_page >}}

# なぜJSONを使うのか
JSONは、データ交換や保存の形式として広く使われており、多くのプログラマーにとって非常に便利なツールです。C++を使ってJSONを扱うことで、プログラムの柔軟性と拡張性を高めることができます。

## 手順
まず、プログラムにJSONデータを読み込むために、`JSON`ライブラリをインポートする必要があります。次に、データを格納するために`json`オブジェクトを作成します。その後、`json`オブジェクトにデータを追加することで、JSON形式のデータを作成することができます。最後に、`json`オブジェクトを文字列として書き出すことで、JSONファイルを作成することができます。

例えば、次のコードは`person`オブジェクトを作成し、`name`と`age`のデータを追加し、JSONファイルとして書き出す例です。

```C++
#include <iostream>
#include "json.hpp"

using namespace std;
using json = nlohmann::json;

int main() {
    // jsonオブジェクトを作成
    json person;
    
    // データを追加
    person["name"] = "John";
    person["age"] = 25;
    
    // 文字列として書き出し
    cout << person.dump() << endl;
    
    return 0;
}
```

このコードを実行すると、次のようなJSONファイルが作成されます。

```json
{
    "name": "John",
    "age": 25
}
```

## 深堀り
JSONは、複雑なデータ構造を持つことができます。例えば、配列やネストされたオブジェクトなど、様々な形式のデータを格納することができます。また、`json`オブジェクトのメソッドを使用することで、データの取得や変更、削除など、さまざまな操作を行うことができます。

さらに、`json`ライブラリには、JSONデータのバリデーションやファイルの読み書き、データの比較など、便利な機能がたくさんあります。プログラマーの皆さんは、是非ともJSONを使ったプログラミングに挑戦してみてください！

## 関連リンク
- [JSONライブラリ(nlohmann/json)](https://github.com/nlohmann/json)
- [C++でJSONを扱う方法(阿吽)](https://www.atmarkit.co.jp/ait/articles/1909/06/news032.html)
- [JSON入門 (Qiita)](https://qiita.com/tbpgr/items/989c6badefff69377da7)
- [JSON形式を学ぶ (Mozilla)](https://developer.mozilla.org/ja/docs/Learn/JavaScript/Objects/JSON)