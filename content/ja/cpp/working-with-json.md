---
title:                "「JSONでの作業」"
html_title:           "C++: 「JSONでの作業」"
simple_title:         "「JSONでの作業」"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## 何をするの？  &  なぜするの？
JSONとは、JavaScript Object Notationの略称で、データを表現するためのフォーマットです。プログラマーはJSONを使用することで、データを簡潔な形式で扱うことができ、さまざまなプログラミング言語やプラットフォームで互換性のあるデータ形式を作成することができます。

## 方法：
```C++

#include <iostream>
#include <json.hpp>

using namespace std;
using json = nlohmann::json;

int main() {

// JSONデータを作成する
json data = {
    {"name", "山田太郎"},
    {"age", 25},
    {"city", "東京"}
};

// JSONデータを画面に表示する
cout << data << endl;

// nameの値を取得する
cout << "名前：" << data["name"] << endl;

// JSONをファイルに保存する
ofstream file("data.json");
file << data;
file.close();

return 0;
}
```
### 出力：
```
{
  "name": "山田太郎",
  "age": 25,
  "city": "東京"
}

名前：山田太郎
```

## 詳しく見る：
JSONは、1999年にDouglas Crockfordによって開発された軽量でシンプルなデータ形式です。XMLなどの他のフォーマットと比べると、より読みやすく、扱いやすいため、Webアプリケーションやモバイルアプリなどのデータ交換形式として広く使われています。

JSONには、規格や構文が定められているわけではありません。そのため、自由度の高いデータ形式と言えます。また、JSONを扱うライブラリは多く存在し、JavaScriptだけではなく、C++やPythonなどの言語でも使用することができます。

## 関連リンク：
- [JSON公式サイト](https://www.json.org/json-ja.html)
- [JSON入門](https://techacademy.jp/magazine/14242)