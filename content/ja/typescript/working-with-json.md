---
title:                "「JSONを使ったプログラミング」"
html_title:           "TypeScript: 「JSONを使ったプログラミング」"
simple_title:         "「JSONを使ったプログラミング」"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## なぜ

JSON（JavaScript Object Notation）は、ウェブ開発やAPI通信など、多くの場面で必要とされるデータ形式です。JavaScriptやTypeScriptのプログラミングにおいて、JSONを扱えるようになることで、より多くの機能を実現することができます。

## 使い方

```TypeScript
// JSONのデータの作成
const person = {
  name: "太郎",
  age: 24,
  hobbies: ["スポーツ", "音楽", "旅行"],
};

// JSONのデータを文字列に変換
const jsonString = JSON.stringify(person);

// 文字列からJSONデータに変換
const personObject = JSON.parse(jsonString);
console.log(personObject); // { name: "太郎", age: 24, hobbies: ["スポーツ", "音楽", "旅行"]}
```

### Output:

```TypeScript
JSONデータを文字列に変換するために、`JSON.stringify()`を使用することができます。また、文字列からJSONデータに変換するためには、`JSON.parse()`を使用します。これらのメソッドを使うことで、オブジェクトを簡単にJSONデータに変換し、処理することができます。

## 深堀り

JSONデータには、オブジェクトや配列の他にも、数値や文字列など様々なデータ型を含めることができます。また、`.json`ファイルとして外部ファイルに保存することもでき、プログラム内でそのデータを読み込むことができます。

## おわりに

### 参考リンク

- [MDN Web Docs: Working with JSON](https://developer.mozilla.org/ja/docs/Learn/JavaScript/Objects/JSON)
- [TypeScript 日本語ドキュメント: JSON サポート](https://typescript-jp.gitbook.io/deep-dive/type-system/json)
- [JSONを活用してTypeScriptのしっかりとした型定義を行うための手書き型定義 - Qiita](https://qiita.com/takamii228/items/015745b4a80faaba1ede)