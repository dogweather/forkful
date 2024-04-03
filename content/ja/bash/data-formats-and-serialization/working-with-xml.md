---
date: 2024-01-26 04:27:50.009732-07:00
description: "\u65B9\u6CD5\uFF1A Bash\u3067XML\u3092\u89E3\u6790\u3059\u308B\u65B9\
  \u6CD5\u3067\u3059\u3002\u30C4\u30FC\u30EB\u306F\uFF1Fxmllint\u3068xmlstarlet\u3067\
  \u3059\u3002XML\u8981\u7D20\u3092\u30EB\u30FC\u30D7\u51E6\u7406\u3059\u308B\u3053\
  \u3068\uFF1F\u9593\u9055\u3044\u306A\u3057\u3067\u3059\u3002\u30B5\u30F3\u30D7\u30EB\
  \u51FA\u529B\u4ED8\u304D\u306E\u4F8B\uFF1A."
lastmod: '2024-03-13T22:44:42.407590-06:00'
model: gpt-4-0125-preview
summary: "Bash\u3067XML\u3092\u89E3\u6790\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\
  \u30C4\u30FC\u30EB\u306F\uFF1Fxmllint\u3068xmlstarlet\u3067\u3059\u3002XML\u8981\
  \u7D20\u3092\u30EB\u30FC\u30D7\u51E6\u7406\u3059\u308B\u3053\u3068\uFF1F\u9593\u9055\
  \u3044\u306A\u3057\u3067\u3059\u3002\u30B5\u30F3\u30D7\u30EB\u51FA\u529B\u4ED8\u304D\
  \u306E\u4F8B\uFF1A."
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

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
