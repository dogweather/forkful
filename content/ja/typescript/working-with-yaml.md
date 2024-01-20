---
title:                "YAMLでの作業"
html_title:           "TypeScript: YAMLでの作業"
simple_title:         "YAMLでの作業"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## YAMLとは？

YAMLとは、プログラマーが使用するデータ表現言語の一つです。主に設定ファイルや構成ファイルをシンプルに書くことができます。プログラマーがYAMLを使う理由は、コードを見やすく保守性を高めるためです。

## やり方：

以下のように、TypeScriptでYAMLを使う方法を説明します。

```TypeScript
import * as yaml from "js-yaml";
import * as fs from "fs";

// YAMLファイルを読み込みます
const yamlFile = fs.readFileSync("./sample.yaml", "utf8");

// YAMLをオブジェクトに変換します
const yamlObject = yaml.safeLoad(yamlFile);

// オブジェクトをYAMLに変換し、新しいファイルに書き込みます
const newYaml = yaml.safeDump(yamlObject);
fs.writeFileSync("./newSample.yaml", newYaml, "utf8");
```

## 深堀り：

YAMLは、XMLやJSONに比べてシンプルで読みやすい表現ができるため、主に設定ファイルや構成ファイルとして使用されてきました。しかし、最近ではJSONやGraphQLがより人気になってきています。それでも、YAMLは依然として多くのプロジェクトで使われています。

## 参考：

- [YAML 公式サイト](https://yaml.org/)
- [js-yaml Library](https://github.com/nodeca/js-yaml)