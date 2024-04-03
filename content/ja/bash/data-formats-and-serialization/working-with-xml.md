---
date: 2024-01-26 04:27:50.009732-07:00
description: "XML\u3092\u6271\u3046\u3053\u3068\u306B\u306F\u3001\u89E3\u6790\u3001\
  \u62BD\u51FA\u3001\u305D\u3057\u3066\u62E1\u5F35\u30DE\u30FC\u30AF\u30A2\u30C3\u30D7\
  \u8A00\u8A9E\u5F62\u5F0F\u3067\u306E\u30C7\u30FC\u30BF\u64CD\u4F5C\u304C\u542B\u307E\
  \u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8A2D\u5B9A\
  \u3001API\u306A\u3069\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u3068\u3057\u3066\u5E83\u304F\u4F7F\u308F\u308C\u3066\u3044\u308B\u305F\
  \u3081\u3001XML\u3068\u683C\u95D8\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.407590-06:00'
model: gpt-4-0125-preview
summary: "XML\u3092\u6271\u3046\u3053\u3068\u306B\u306F\u3001\u89E3\u6790\u3001\u62BD\
  \u51FA\u3001\u305D\u3057\u3066\u62E1\u5F35\u30DE\u30FC\u30AF\u30A2\u30C3\u30D7\u8A00\
  \u8A9E\u5F62\u5F0F\u3067\u306E\u30C7\u30FC\u30BF\u64CD\u4F5C\u304C\u542B\u307E\u308C\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8A2D\u5B9A\u3001\
  API\u306A\u3069\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u30D5\u30A9\u30FC\u30DE\u30C3\
  \u30C8\u3068\u3057\u3066\u5E83\u304F\u4F7F\u308F\u308C\u3066\u3044\u308B\u305F\u3081\
  \u3001XML\u3068\u683C\u95D8\u3057\u307E\u3059\u3002."
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

## 何となぜ？
XMLを扱うことには、解析、抽出、そして拡張マークアップ言語形式でのデータ操作が含まれます。プログラマーは、設定、APIなどのデータ交換フォーマットとして広く使われているため、XMLと格闘します。

## 方法：
BashでXMLを解析する方法です。ツールは？xmllintとxmlstarletです。XML要素をループ処理すること？間違いなしです。サンプル出力付きの例：

```bash
# xmlstarletがインストールされていると仮定
# インストールするには：apt-get install xmlstarlet

# XMLコンテンツを解析
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# xmlstarletで名前を抽出
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# 出力は以下のようになります：
# Apple
# Banana
```

## 深く掘り下げる
90年代に入ると、XMLはSGMLに比べてよりシンプルな代替品として登場しましたが、HTMLよりも構造化されています。現在では、例えばJSON、YAMLなどの仲間がいます。しかし、XMLは特に設定やSOAPベースのWebサービスで、依然として活躍しています。

ツールの面では、xmllintはXMLの検証、xpathクエリに適しています。xmlstarletはXMLのさまざまなことに対応するスイスアーミーナイフです―クエリ、編集、検証、変換。Bashスクリプトでは、XMLタスクのためのスーパーヒーローです。

内部では、xmllintはlibxml2―XML Cパーサーを使用しています。それは速いですが、エラーメッセージは？解読しにくいです。そしてxmlstarlet？再帰テンプレートとEXSLTサポート。頭を悩ませるかもしれませんが、非常に強力です。

## 参照
- [xmlsoft.org](http://xmlsoft.org/)：Libxml2とxmllintの情報。
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash)：実世界の問題と解決策。
- [W3Schools XMLチュートリアル](https://www.w3schools.com/xml/)：XMLの基礎。
