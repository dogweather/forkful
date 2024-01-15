---
title:                "「YAMLを使ったプログラミング」"
html_title:           "Gleam: 「YAMLを使ったプログラミング」"
simple_title:         "「YAMLを使ったプログラミング」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜ？

YAMLとは、人間にとって読みやすく、コンピューターにとっても解析しやすいテキスト形式のデータ保存方法です。GleamでYAMLを扱うことで、よりシンプルで効率的なコーディングが可能になります。

## 使い方

```Gleam
import gleam/yaml.{decode, encode}

// YAMLをデコードする例
let yaml = "name: John\nage: 30"
let person = yaml
  |> decode
  // Result<Person, DecodeError>型に変換
  |> Result.map_err(fn err => err |> to_string |> yaml.error)

// YAMLをエンコードする例
let user = %{
  name: "Emily",
  email: "emily@example.com"
}
user
  |> encode
  // Result<String, EncodeError>型に変換
  |> Result.map_err(fn err => err |> to_string |> yaml.error)
  // 結果は "name: Emily\nemail: emily@example.com" のようになります
```

## 詳細を深堀り

YAMLを使用することで、データを階層構造で表現することができ、複雑なオブジェクトやリストを簡潔に表現することができます。さらに、Gleamのパターンマッチング機能を使用することで、より複雑なデータのパースが可能になります。また、YAMLとJSONの相互変換もサポートしています。

## See Also

- YAMLの公式サイト: https://yaml.org/
- Gleamの公式ドキュメント: https://gleam.run/