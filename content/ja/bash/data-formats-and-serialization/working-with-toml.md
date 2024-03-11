---
date: 2024-01-26 04:19:03.600767-07:00
description: "TOML\uFF08Tom's Obvious, Minimal Language\u306E\u7565\uFF09\u306F\u3001\
  \u30C7\u30FC\u30BF\u76F4\u5217\u5316\u5F62\u5F0F\u3067\u3059\u3002\u305D\u306E\u30B7\
  \u30F3\u30D7\u30EB\u3055\u3068\u53EF\u8AAD\u6027\u304B\u3089\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306B\u597D\u307E\u308C\u3066\u304A\u308A\u3001\u8A2D\u5B9A\u30D5\u30A1\
  \u30A4\u30EB\u306B\u6700\u9069\u3067\u3059\u3002YAML\u306B\u4F3C\u305F\u96F0\u56F2\
  \u6C17\u3092\u6301\u3061\u306A\u304C\u3089\u3082\u3001\u4EBA\u9593\u306B\u3068\u3063\
  \u3066JSON\u3088\u308A\u3082\u5384\u4ECB\u3055\u304C\u5C11\u306A\u3044\u3067\u3059\
  \u3002"
lastmod: '2024-03-11T00:14:15.959268-06:00'
model: gpt-4-0125-preview
summary: "TOML\uFF08Tom's Obvious, Minimal Language\u306E\u7565\uFF09\u306F\u3001\u30C7\
  \u30FC\u30BF\u76F4\u5217\u5316\u5F62\u5F0F\u3067\u3059\u3002\u305D\u306E\u30B7\u30F3\
  \u30D7\u30EB\u3055\u3068\u53EF\u8AAD\u6027\u304B\u3089\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306B\u597D\u307E\u308C\u3066\u304A\u308A\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\
  \u30EB\u306B\u6700\u9069\u3067\u3059\u3002YAML\u306B\u4F3C\u305F\u96F0\u56F2\u6C17\
  \u3092\u6301\u3061\u306A\u304C\u3089\u3082\u3001\u4EBA\u9593\u306B\u3068\u3063\u3066\
  JSON\u3088\u308A\u3082\u5384\u4ECB\u3055\u304C\u5C11\u306A\u3044\u3067\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
---

{{< edit_this_page >}}

## 何となく何故？
TOML（Tom's Obvious, Minimal Languageの略）は、データ直列化形式です。そのシンプルさと可読性からプログラマーに好まれており、設定ファイルに最適です。YAMLに似た雰囲気を持ちながらも、人間にとってJSONよりも厄介さが少ないです。

## どうやって：
まず、BashでTOMLをいじるために`toml-cli`をインストールします。TOMLファイルをその場で読んだり編集したりするのに便利です。

```Bash
# TOMLタスクのための小さなヘルパー、toml-cliをインストールします
pip install toml-cli

# 'config.toml'というTOMLファイルがあるとします
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# 値を読む
toml get config.toml owner.name
# 出力：Tom

# 値を設定する
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# 豆知識：ドットや変な文字が含まれるキーには引用符を使いましょう！
```

## 深掘り
TOMLは、人間にとってのJSONの障壁に対する不満から、2013年頃に登場しました。GitHubの共同創設者であるTom Preston-Wernerは、非常に読みやすい何かを望んでいました。YAMLとINIは代替案でしたが、TOMLはその両方のベストです。

シバン！ネストされたデータや配列を持つことができ、YAMLの落とし穴やJSONの中括弧を回避します。今では、RustのCargoでの設定におけるTOMLの主流であり、それが開発界での隆盛を物語っています。しっかりと定義され、厳格に管理された仕様によって推進されており、ほぼあらゆる言語でパーサーを手に入れることができるため、幅広く採用されています。

## また見る
- 公式TOML GitHubリポジトリ：https://github.com/toml-lang/toml
- PyPI上のtoml-cli：https://pypi.org/project/toml-cli/
- データシリアライゼーション形式の比較：https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
