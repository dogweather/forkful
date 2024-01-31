---
title:                "XMLの扱い方"
date:                  2024-01-26T04:30:45.137847-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"

category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/working-with-xml.md"
---

{{< edit_this_page >}}

## 何となぜ？
XMLを扱うということは、設定、メッセージングなどで使われる普及している構造化されたフォーマットでデータを取り扱うことを意味します。プログラマーはデータを読み、書き、更新、そして問い合わせるためにXMLを操作します—これは多数のアプリやサービスの相互運用性にとって不可欠です。

## 方法：
Fishには組み込みのXML解析がないため、`xmllint`や`xmlstarlet`のような外部ツールに依存します。値を読み取るためのスニペットはこちらです：

```fish
# xmlstarletを使用してXMLを解析
echo '<root><element>Hello World</element></root>' | xmlstarlet sel -t -v "/root/element"
```

出力：
```
Hello World
```

XMLを編集するには、これを使用します：

```fish
# xmlstarletを使用してXML要素を編集
echo '<root><element>Old Value</element></root>' | xmlstarlet ed -u "/root/element" -v 'New Value'
```

出力：
```xml
<?xml version="1.0"?>
<root>
  <element>New Value</element>
</root>
```

## 深掘り：
XMLは90年代後半からある、読みやすさとマシンフレンドリーを目的として作成されました。シンプルさのためにJSONに一部の人気を奪われたものの、文書の検証と名前空間が重要な場所ではXMLが根強く残っています。

代替案？もちろん—JSONやYAML、あるいはパフォーマンス重視のアプリにはProtocol Buffersのようなバイナリフォーマットなど。しかし、XMLのスキーマとXSLT（XML変換のため）は、堅牢さが重要な複雑なシナリオにおいては魅力的です。

内部では、`xmlstarlet`のようなツールはlibxml2のような強力なライブラリをラップし、XMLを細かくいじるためのXPathやXQueryを提供しています。これらは単なるXMLツールではなく、任意の言語でXMLに触れる際に同様のコンセプトを適用できるDOM操作へのゲートウェイでもあります。

## 参照：
- [xmlstarletドキュメント](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Fishドキュメント](https://fishshell.com/docs/current/index.html)
- [XPathとXQueryの関数とオペレーター](https://www.w3.org/TR/xpath-functions/)
