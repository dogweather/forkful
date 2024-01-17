---
title:                "「JSONを使う」"
html_title:           "Gleam: 「JSONを使う」"
simple_title:         "「JSONを使う」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/working-with-json.md"
---

{{< edit_this_page >}}

# JSONを扱うためにGleamを使おう！

こんにちは、みなさん！今回は、Gleamを使ってJSONを扱う方法についてお話しします。JSONとは何か、そしてプログラマーがなぜそれを使うのか、まずは簡単にご説明しましょう。

## 詳しくはどうするの？

JSONは、データを格納してやりとりするための一般的なフォーマットです。プログラマーは、JSONを使うことでデータをより簡単に扱うことができます。例えば、ウェブアプリケーションを開発する際に、サーバーからクライアントにデータを送信する際によく使われます。

## やり方は？

GleamでJSONを扱うには、まずは```json```モジュールをインポートします。

```gleam
import json
```

次に、JavaScriptのようにJSONを解析することができます。例えば、次のコードでは、JSONを文字列から値に変換します。

```gleam
let value = json.parse("{\"name\": \"John\", \"age\": 30}")
```

また、JSONを文字列に変換することもできます。

```gleam
let str = json.stringify({"name": "John", "age": 30})
```

## 深入りする

JSONは、データ交換フォーマットとして1990年代から使用されてきました。しかし、もしあなたがJSONに代わるものをお探しであれば、YAMLやXMLなどの他のフォーマットもあります。

Gleamの```json```モジュールは、一部動的な方法を使用してJSONを解析および生成するため、パフォーマンスが向上します。しかし、Gleamのモジュールではなく、他のライブラリを使用することもできます。

## 関連リンク

- [Gleamの公式ウェブサイト](https://gleam.run/)
- [JSONのドキュメント](https://www.json.org/json-en.html)