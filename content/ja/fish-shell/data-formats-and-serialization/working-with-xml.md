---
date: 2024-01-26 04:30:45.137847-07:00
description: "XML\u3092\u6271\u3046\u3068\u3044\u3046\u3053\u3068\u306F\u3001\u8A2D\
  \u5B9A\u3001\u30E1\u30C3\u30BB\u30FC\u30B8\u30F3\u30B0\u306A\u3069\u3067\u4F7F\u308F\
  \u308C\u308B\u666E\u53CA\u3057\u3066\u3044\u308B\u69CB\u9020\u5316\u3055\u308C\u305F\
  \u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u30C7\u30FC\u30BF\u3092\u53D6\u308A\u6271\
  \u3046\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u3092\u8AAD\u307F\u3001\u66F8\u304D\u3001\u66F4\
  \u65B0\u3001\u305D\u3057\u3066\u554F\u3044\u5408\u308F\u305B\u308B\u305F\u3081\u306B\
  XML\u3092\u64CD\u4F5C\u3057\u307E\u3059\u2014\u3053\u308C\u306F\u591A\u6570\u306E\
  \u30A2\u30D7\u30EA\u3084\u30B5\u30FC\u30D3\u30B9\u306E\u76F8\u4E92\u904B\u7528\u6027\
  \u306B\u3068\u3063\u3066\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
lastmod: '2024-03-11T00:14:16.320743-06:00'
model: gpt-4-0125-preview
summary: "XML\u3092\u6271\u3046\u3068\u3044\u3046\u3053\u3068\u306F\u3001\u8A2D\u5B9A\
  \u3001\u30E1\u30C3\u30BB\u30FC\u30B8\u30F3\u30B0\u306A\u3069\u3067\u4F7F\u308F\u308C\
  \u308B\u666E\u53CA\u3057\u3066\u3044\u308B\u69CB\u9020\u5316\u3055\u308C\u305F\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u30C7\u30FC\u30BF\u3092\u53D6\u308A\u6271\u3046\
  \u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u30C7\u30FC\u30BF\u3092\u8AAD\u307F\u3001\u66F8\u304D\u3001\u66F4\u65B0\
  \u3001\u305D\u3057\u3066\u554F\u3044\u5408\u308F\u305B\u308B\u305F\u3081\u306BXML\u3092\
  \u64CD\u4F5C\u3057\u307E\u3059\u2014\u3053\u308C\u306F\u591A\u6570\u306E\u30A2\u30D7\
  \u30EA\u3084\u30B5\u30FC\u30D3\u30B9\u306E\u76F8\u4E92\u904B\u7528\u6027\u306B\u3068\
  \u3063\u3066\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
title: "XML\u306E\u6271\u3044\u65B9"
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
