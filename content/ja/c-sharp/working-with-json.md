---
title:                "「JSONを使用する」"
html_title:           "C#: 「JSONを使用する」"
simple_title:         "「JSONを使用する」"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## なぜ
JSONを使ってプログラミングをする理由の簡単な説明。

JSONは現代のプログラミングにおいて非常に重要な役割を果たしています。Webアプリケーション、モバイルアプリ、データの保存や取得など、さまざまな場面でJSONを使用することができます。今回は、C#を使ってJSONを扱う方法を見ていきましょう。

## 方法
```C#
// JSONファイルの読み込み
using (StreamReader r = new StreamReader("example.json"))
{
    string json = r.ReadToEnd();
    // JSONをオブジェクトに変換
    dynamic data = JsonConvert.DeserializeObject(json);

    // オブジェクトからデータを取得
    string name = data["name"];
    int age = data["age"];
    string[] hobbies = data["hobbies"];

    // データの出力
    Console.WriteLine("名前：" + name);
    Console.WriteLine("年齢：" + age);
    Console.Write("趣味：");
    foreach (string hobby in hobbies)
    {
        Console.Write(hobby + ", ");
    }
}
```

上記のコードでは、まずJSONファイルを読み込み、`JsonConvert`を使ってその内容をオブジェクトに変換しています。その後、オブジェクトから必要なデータを取得して出力することで、簡単にJSONを扱うことができます。

## 深堀り
JSONを扱う上で、より高度な操作が必要になる場合もあります。そのような場合には、JSONを扱うための専用のライブラリが存在します。例えば、JSONのバリデーションやフォーマットの変更などを行う場合には、`Newtonsoft.Json`ライブラリが便利です。また、`System.Net.Http`ライブラリを使うことで、WebサービスからJSONデータを取得することができます。

## 参考リンク
- [C#でJSONを扱う方法](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/concepts/serialization/how-to-read-json-from-a-file)
- [Newtonsoft.Jsonライブラリの使い方](https://www.newtonsoft.com/json/help/html/SerializationGuide.htm)
- [System.Net.Httpライブラリの使い方](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=netcore-3.1)