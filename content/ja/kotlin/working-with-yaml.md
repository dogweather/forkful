---
title:                "「yamlを使う方法」"
html_title:           "Kotlin: 「yamlを使う方法」"
simple_title:         "「yamlを使う方法」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜ

YAMLを使用することのメリットはたくさんあります。例えば、設定ファイルやデータのフォーマットに使用することで、人間にとっても読みやすい構文を提供し、コンピューターにとっても解析しやすい形式でデータを保存できます。

## 使い方

まずは、KotlinでYAMLを扱うために必要なライブラリをプロジェクトに追加しましょう。次のコードを`build.gradle`ファイルの`dependencies`に追加します。

```
dependencies {
    implementation 'net.jodah:exp4j:0.4.8'
}
```

次に、必要なライブラリをインポートします。

```Kotlin
import org.yaml.snakeyaml.Yaml
import java.io.File
```

Yamlインスタンスを作成し、YAMLファイルを読み込みます。

```Kotlin
val yaml = Yaml()
val data = yaml.load(File("sample.yaml").inputStream()) as Map<String, Any>
```

データを操作したい場合は、`data`変数を使ってデータを取得または更新することができます。

```Kotlin
val name = data["name"] // "John Doe"が返される
data["age"] = 25 // dataに"age: 25"のキーと値が追加される
```

YAMLファイルを書き出す場合は、以下のように行うことができます。

```Kotlin
val yamlString = yaml.dump(data) // YAML形式の文字列が生成される
```

## 詳細を調べる

YAMLを扱うためのKotlinライブラリとしては、他にも`SnakeYAML`や`speedment-json`があります。YAMLは非常に人間にとって読みやすいフォーマットであり、複数のプログラミング言語で利用されています。しかし、YAMLはデータをシリアライズするためのものであり、特定の用途に合わせたデータの構造を定義することができます。

## さらに見る

- [SnakeYAML Githubリポジトリ](https://github.com/asomov/snakeyaml)
- [speedment-json Githubリポジトリ](https://github.com/speedment/speedment-json)