---
title:                "XMLの扱い方"
date:                  2024-01-26T04:27:50.009732-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-xml.md"
---

{{< edit_this_page >}}

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