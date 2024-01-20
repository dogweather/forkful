---
title:                "YAMLを扱う"
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
YAMLは設定やデータを表すためのフォーマット。読みやすく、編集もしやすいため、開発者がよく使う。

## How to: (やり方)
KotlinでYAMLを扱うには、専用のライブラリが必要。例えば`snakeyaml`を使うことができる。

```kotlin
import org.yaml.snakeyaml.Yaml

fun main() {
    val yaml = Yaml()
    val data = "name: Yuto\nage: 25"
    val parsedData = yaml.load<Map<String, Any>>(data)

    println(parsedData) // {name=Yuto, age=25}
}
```

`snakeyaml`ライブラリを使うと、YAML形式のテキストがKotlinのマップに変換される。

## Deep Dive (深掘り)
YAMLは"YAML Ain't Markup Language"の略。JSONやXMLと比べ、人間が読み書きしやすい。しかし、パース時のエラーが起こりやすいのが難点。JSONやTOMLも良い選択肢。

## See Also (関連リンク)
- YAML公式サイト: [https://yaml.org/](https://yaml.org/)