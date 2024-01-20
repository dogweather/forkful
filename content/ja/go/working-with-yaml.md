---
title:                "YAMLでの作業"
html_title:           "Go: YAMLでの作業"
simple_title:         "YAMLでの作業"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## なに & どうして?

YAMLとは、プログラミング言語でデータを表現するための構造化されたフォーマットです。プログラマーはYAMLを使用することで、データを簡単に扱いやすくすることができます。

## 使い方:

```Go
import "gopkg.in/yaml.v2"
```
を使用することで、Goプログラミング言語でYAMLを取り扱うことが可能です。以下は、YAMLを読み込んでデコードし、JSON形式に変換するコードの例です。

```Go
yamlData := `name: John
age: 25`
var data map[string]interface{}
err := yaml.Unmarshal([]byte(yamlData), &data)

if err != nil {
  fmt.Println(err)
}
json, err := json.Marshal(data)

if err != nil {
  fmt.Println(err)
}
fmt.Println(string(json))
 ```

実行結果:

```Go
{"name":"John","age":25}
```

## 深堀り:

YAMLは、XMLやJSONなどのデータフォーマットの一つです。その最大の特徴は、人間にとって読みやすく扱いやすいことです。しかし、他のフォーマットと比べるとパフォーマンスが低く、複雑なデータを扱う際には向いていません。

代替としては、JSONやXMLなどがあります。これらのフォーマットは、パフォーマンスが高く、より複雑なデータを扱うことができます。しかし、人間にとって読みやすくないという欠点があります。

YAMLの実装には、多くのオプションがありますが、私たちが使用したのはgopkg.in/yaml.v2でした。これは、Go言語で最もよく使用されるYAMLライブラリの一つです。

## 関連情報:
