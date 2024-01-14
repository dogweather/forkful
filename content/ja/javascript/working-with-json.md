---
title:                "Javascript: 「JSONを扱う」"
simple_title:         "「JSONを扱う」"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## なぜJSONを使うのか？

JSONはJavaScript Object Notationの略で、ネット上でデータをやり取りする際に非常に便利です。記事では、JSONを使うメリットや基本的な使い方を紹介します。

## JSONの基本的な使い方

まずは、JSONを使ってデータを取得する方法を学びましょう。
```Javascript
let jsonExample = `{
  "name": "John",
  "age": 30,
  "hobbies": ["reading", "playing games", "watching movies"],
  "address": {
    "city": "Tokyo",
    "country": "Japan"
  }
}`;

let parsedJson = JSON.parse(jsonExample);
console.log(parsedJson.name); // output: "John"
console.log(parsedJson.hobbies[1]); // output: "playing games"
console.log(parsedJson.address.country); // output: "Japan"
```
上記のコードでは、変数jsonExampleにJSON形式のデータを代入し、JSON.parse()メソッドを使ってオブジェクトに変換しています。その後、オブジェクトのプロパティを使ってデータを取得しています。

次に、JSONを使ってデータを送信する方法を見てみましょう。
```Javascript
let sendData = {
  "name": "Jane",
  "age": 25,
  "hobbies": ["playing guitar", "cooking", "traveling"],
  "address": {
    "city": "Osaka",
    "country": "Japan"
  }
};

let jsonString = JSON.stringify(sendData);
console.log(jsonString); // output: "{"name":"Jane","age":25,"hobbies":["playing guitar","cooking","traveling"],"address":{"city":"Osaka","country":"Japan"}}"

// ここで、jsonStringをネット上で送信するなどしてデータをやり取りします
```
上記のコードでは、オブジェクトをJSON形式の文字列に変換するためにJSON.stringify()メソッドを使っています。これを使うことで、データをやり取りする際にJSON形式を使うことができます。

## JSONの詳細

JSONはJavaScriptのオブジェクトの形式を取り入れたデータ形式です。そのため、JavaScriptの文法に沿ってデータを記述することができます。また、JSONは他のプログラミング言語でも使われており、データをやり取りする際に非常に便利です。

さらに、JSONは人間でも読みやすい形式であり、コンピューターでも解析しやすい形式です。そのため、データの取得や送信がスムーズに行えるように設計されています。

## その他のリンク

- [JSONの公式ドキュメント](https://www.json.org/json-ja.html)
- [JSONを使ったデータの送受信方法](https://developer.mozilla.org/ja/docs/Learn/JavaScript/Objects/JSON)
- [JSON vs XML: どちらを使うべきか](https://www.smashingmagazine.com/2016/09/json-vs-xml-which-one-is-right-for-you/)
- [JSONのエディターやビューアーのリスト](https://www.json.org/json-es.html)