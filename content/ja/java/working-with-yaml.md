---
title:                "yamlでの作業"
html_title:           "Java: yamlでの作業"
simple_title:         "yamlでの作業"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/working-with-yaml.md"
---

{{< edit_this_page >}}

##　なにがし & なんで？
YAML とは何か？プログラマーがそれを行う理由はなんですか？
YAML（ヤムル）とは、プログラマーがデータを記述するために使用するファイルの形式のことです。このファイル形式では、人間が読みやすく書くことができ、コンピューターがパース（解析）することも容易です。プログラマーがYAMLを操作する主な理由は、設定ファイルやデータファイルを作成し、読み込むことが簡単であることです。

## 作り方：
 `Java ... `のコードブロック内のコーディング例とサンプル出力。

作り方の例:
```
Yaml yaml = new Yaml();
InputStream inputStream = new FileInputStream(new File("example.yaml"));

//YAMLファイルを読み込み、Javaのオブジェクトにマッピングする
Object obj = yaml.load(inputStream); 

//オブジェクトをYAML形式のテキストに変換する
System.out.println(yaml.dump(obj));
```

出力の例:
```
foo:
 - baz
 - 2
 - hello there
msg: hello!
```

## 深く掘り下げる:
(1) 歴史的文脈、(2) 代替手段、(3) YAMLを操作する際の実装の詳細など、深めの情報
 

### 歴史的文脈：
YAMLは、Perlスクリプト言語の作者であるIngy döt Netによって提案されたものです。彼の目的は、JSONやXMLと同様に複数の言語間で使われる汎用のデータ交換フォーマットを作ることでした。その後、その目的は達成され、YAMLは広く使用されるようになりました。

### 代替手段:
YAMLを扱うための代替手段として、JSONやXMLなどのフォーマットがあります。しかし、これらのフォーマットは構造化されたデータを扱う際には、YAMLよりも冗長な記述が必要です。

### 実装の詳細:
YAML形式のファイルをJavaで操作するためには、SnakeYAMLやYAMLBeansなどのライブラリを使用することができます。これらのライブラリを使用すると、アプリケーション内でYAMLファイルを直接操作することができます。

## 関連情報:
- [YAML 公式サイト](https://yaml.org/)
- [YAMLBeans ライブラリ](https://yamlbeans.sourceforge.net/)
- [SnakeYAML ライブラリ](https://bitbucket.org/asomov/snakeyaml)

このように、YAMLを使用することで、プログラムの設定やデータを簡単に保存し、読み込むことができます。Javaでは、様々なライブラリを使用して、YAMLを操作することができるので、ぜひ活用してみてください。