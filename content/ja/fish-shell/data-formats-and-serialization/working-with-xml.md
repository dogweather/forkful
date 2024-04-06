---
date: 2024-01-26 04:30:45.137847-07:00
description: "\u65B9\u6CD5\uFF1A Fish\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306EXML\u89E3\
  \u6790\u304C\u306A\u3044\u305F\u3081\u3001`xmllint`\u3084`xmlstarlet`\u306E\u3088\
  \u3046\u306A\u5916\u90E8\u30C4\u30FC\u30EB\u306B\u4F9D\u5B58\u3057\u307E\u3059\u3002\
  \u5024\u3092\u8AAD\u307F\u53D6\u308B\u305F\u3081\u306E\u30B9\u30CB\u30DA\u30C3\u30C8\
  \u306F\u3053\u3061\u3089\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.550756-06:00'
model: gpt-4-0125-preview
summary: ''
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

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
