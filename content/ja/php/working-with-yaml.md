---
title:                "「yamlとの作業」"
html_title:           "PHP: 「yamlとの作業」"
simple_title:         "「yamlとの作業」"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## なに？ なぜ？
YAMLとは、プログラマーがデータを表現するためのフォーマットです。プログラマーはYAMLを使用することで、よりシンプルに、かつ読みやすい形式でデータを保存・転送できます。

## 使い方：
```PHP
$data = [
    'name' => 'John',
    'age' => 25,
    'favorite_foods' => ['pizza', 'sushi', 'tacos'] 
];

// YAMLフォーマットに変換する
$yaml = yaml_emit($data);
echo $yaml;
// 出力結果：
// name: John
// age: 25
// favorite_foods:
//   - pizza
//   - sushi
//   - tacos
```

## 詳細:
YAMLは、2001年にPerl開発者のIngy döt Netによって開発されました。XMLやJSONなどの他のデータフォーマットに比べて、より人間にとって読みやすい形式でデータを表現できます。また、拡張性も高く、他のプログラムや言語でも利用することができるため、広く使われています。

YAMLの代替としては、JSONやXMLなどがありますが、YAMLはよりコンパクトかつ視覚的に理解しやすいのが特徴です。また、PHPでは内部でYAMLライブラリが使用されているため、追加の設定なしで使用することができます。

## 関連情報：
- [公式YAMLドキュメント](https://yaml.org/)
- [PHPのYAMLライブラリドキュメント](https://www.php.net/manual/en/book.yaml.php)
- [YAML vs. JSON](https://www.educba.com/yaml-vs-json/)
- [YAML vs. XML](https://www.guru99.com/yaml-vs-xml-difference.html)