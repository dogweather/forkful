---
title:                "C#: 「JSON を使ったプログラミング」"
simple_title:         "「JSON を使ったプログラミング」"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

私たちが日々コンピューターと取引しているデータは、しばしばJSONという形式で保存されます。JSONは、人間が読みやすいテキスト形式で構造化されたデータを表すために使用されます。JSONを扱うことで、データの取得や処理が容易になります。そこで今回は、C#を使用してJSONを扱う方法についてご紹介します。

## Why

JSONは現在、多くのアプリケーションやウェブサイトで使用されています。そのため、C#を使用してJSONを扱うことで、さまざまなプロジェクトに参加することができます。また、JSONを取得して処理することで、より柔軟性が高く、効率的なコードを作成することができます。

## How To

まずはじめに、JSONを扱うためにはNewtonsoft JsonというC#用のライブラリを使用する必要があります。これを使用することで、JSONファイルからデータを読み込み、オブジェクトに変換することができます。

```
//クラスを作成
public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}

//JSONファイルからデータを読み込み、オブジェクトに変換
string jsonFile = File.ReadAllText("person.json");

Person person = JsonConvert.DeserializeObject<Person>(jsonFile);
Console.WriteLine("名前：" + person.Name);
Console.WriteLine("年齢：" + person.Age);
```

上記のコードでは、"person.json"という名前のファイルからデータを取得し、Personクラスのオブジェクトに変換しています。その後、名前と年齢を出力しています。

また、JSONを扱う際には、独自のオブジェクトを作成する必要があります。これは、データをより具体的に表現するために重要です。例えば、以下のようなJSONファイルがあったとします。

```
{
    "car": {
        "make": "Toyota",
        "model": "Corolla",
        "year": 2020
    }
}
```

このファイルを読み込み、オブジェクトに変換するには、以下のようにコードを書きます。

```
//クラスを作成
public class Car
{
    public string Make { get; set; }
    public string Model { get; set; }
    public int Year { get; set; }
}

//JSONファイルからデータを読み込み、オブジェクトに変換
string jsonFile = File.ReadAllText("car.json");

Car car = JsonConvert.DeserializeObject<Car>(jsonFile);
Console.WriteLine("メーカー：" + car.Make);
Console.WriteLine("モデル：" + car.Model);
Console.WriteLine("年式：" + car.Year);
```

ここでは、Carクラスを作成し、その中にmake、model、yearのプロパティを定義しています。その後、ファイルを読み込み、オブジェクトに変換しています。このように、独自のオブジェクトを作成することで、より簡潔で読みやすいコードを書くことができます。

## Deep Dive

JSONを扱う際に、より深い理解が必要なトピックの一つに、シリアライズとデシリアライズがあります。シリアライズとは、データをファイルやバイト列などの外部形式に変換することを指し、デシリ