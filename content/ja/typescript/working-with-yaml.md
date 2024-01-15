---
title:                "「yamlとの作業」"
html_title:           "TypeScript: 「yamlとの作業」"
simple_title:         "「yamlとの作業」"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why
YAMLは人気があります。なぜなら、YAMLは人間にとって読みやすく論理的なデータ構造を持っており、コンピューターにとっても理解しやすいためです。

## How To
YAMLを使うためには、まずはTypeScriptでYAMLライブラリーをインストールします。
```TypeScript
npm install yaml
```
そして、YAMLファイルを読み込んで解析するには次のようなコードを使います。
```TypeScript
const yaml = require('yaml');
const yamlString = `
name: John
age: 27 
`;
const parsedYaml = yaml.parse(yamlString);
console.log(parsedYaml.name); 
```
上記のコードでは、YAMLのparseメソッドを使ってオブジェクトに変換することで、YAMLのデータを取得することができます。

## Deep Dive
YAMLを扱う際によく使われる機能として、マルチラインデータやエイリアスがあります。マルチラインデータを使うと、複数行にまたがるデータを簡単に表現することができます。また、エイリアスを使うことで同じデータを複数の場所で参照することができます。さらにYAMLはコンパクトでスペースを使ったインデント形式でデータを表現するため、読みやすくネストされたデータ構造を作ることができます。

## See Also
- [TypeScriptでYAMLをパースする方法](https://qiita.com/watcher0226/items/f368063a606fd9c2b074)
- [YAMLの基本文法](https://www.lifewithpython.com/2014/05/yaml-basic-syntax.html)
- [YAML公式ドキュメント](https://yaml.org/spec/)