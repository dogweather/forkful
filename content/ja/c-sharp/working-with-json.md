---
title:                "jsonの扱い方"
html_title:           "C#: jsonの扱い方"
simple_title:         "jsonの扱い方"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## 何かとは？
JSONとは、データのシンプルで軽量なフォーマットの一つです。開発者は、JSONを使用することで、データを直感的に扱うことができ、より柔軟なアプリケーションを作成することができます。

## 方法：
JSONをC#で扱う方法を解説します。まずは、Newtonsoft.Jsonパッケージをダウンロードし、プロジェクトに追加します。次に、以下のようにJSONを読み込んでオブジェクトを作成することができます。 

```C#
//JSONを読み込んでオブジェクトに変換
string json = System.IO.File.ReadAllText(@"path/to/json/file.json");
var myObject = JsonConvert.DeserializeObject<MyClass>(json);
```

また、逆にオブジェクトをJSON形式に変換することも可能です。以下のように書くことで、オブジェクトをJSON文字列に変換できます。

```C#
//オブジェクトをJSON文字列に変換
MyClass myObject = new MyClass();
string json = JsonConvert.SerializeObject(myObject);
```

## 深堀り：
JSONは、JavaScript Object Notationの略で、JavaScriptのオブジェクト表記法をベースに作られたフォーマットです。近年、Webアプリケーションの人気が高まるにつれ、バックエンドとフロントエンド間でデータをやりとりする際に使用されるようになりました。代表的な代替フォーマットとしては、XMLがありますが、JSONはよりシンプルで簡潔な記述ができるため、より広く使われています。なお、C#では他にもJSONを扱うためのライブラリがありますが、Newtonsoft.Jsonは最も広く使われているものです。

## 関連情報：
Newtonsoft.Json公式ページ：[https://www.newtonsoft.com/json](https://www.newtonsoft.com/json)

JSONの詳細については、W3Cのドキュメントを参照してください：[https://www.w3.org/TR/REC-json/](https://www.w3.org/TR/REC-json/)

C#のGitHubリポジトリには、JSONを扱うための標準ライブラリが含まれています：[https://github.com/dotnet/corefx/blob/master/src/System.Text.Json/src/System/Text/Json/](https://github.com/dotnet/corefx/blob/master/src/System.Text.Json/src/System/Text/Json/)