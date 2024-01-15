---
title:                "yamlを扱う"
html_title:           "Javascript: yamlを扱う"
simple_title:         "yamlを扱う"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

今日は最新のJavaScriptプログラミングについてお話しします！この記事では、YAMLというデータフォーマットについてご紹介します。読者の皆様がこの記事を読んで、YAMLを使ってのプログラミングが楽しくなることを願っています。

## なぜ

YAMLは、データを簡潔かつわかりやすく表現するために開発されました。そのため、JavaScriptプログラミングにおいても、コードをより簡潔に書くことができます。また、YAMLは人間にとっても読みやすいため、チームでの開発やコミュニケーションにも役立ちます。

## 使い方

```Javascript
const yaml = require('yaml');

// オブジェクトをYAML形式に変換
const myObject = {
    name: 'John',
    age: 26,
    city: 'Tokyo'
}
const yamlObject = yaml.stringify(myObject);
console.log(yamlObject);

// YAML形式のデータをオブジェクトに変換
const newYaml = `
name: Emma
age: 29
city: Osaka
`;
const newObject = yaml.parse(newYaml);
console.log(newObject);
```

上記のコードを実行すると、次のような出力が得られます。

```Javascript
name: John
age: 26
city: Tokyo

{ 
  name: 'Emma',
  age: 29,
  city: 'Osaka'
}
```

YAMLを使用することで、よりシンプルで読みやすいコードを書くことができるようになります。

## 深層を掘る

YAMLは、データの階層構造を示すためにインデントを使用します。また、配列やオブジェクトを簡単に表現することができます。さらに、変数を使用することもできます。しかし、改行やインデントの数などの細かいルールがあり、正しい形式で書かなければエラーが起きることもあるので注意が必要です。

## 併せて見る

- [YAML公式サイト](https://yaml.org/)
- [YAML入門ガイド](https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html)
- [JavaScriptでのYAMLの基本操作](https://www.npmjs.com/package/yaml)