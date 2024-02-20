---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:40.113077-07:00
description: "Google Apps Script\u3067HTML\u3092\u89E3\u6790\u3059\u308B\u3068\u306F\
  \u3001HTML\u30B3\u30F3\u30C6\u30F3\u30C4\u304B\u3089\u30C7\u30FC\u30BF\u3092\u62BD\
  \u51FA\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u308C\
  \u306F\u3001Web\u30DA\u30FC\u30B8\u3084Web\u30D9\u30FC\u30B9\u306E\u30C7\u30FC\u30BF\
  \u30BD\u30FC\u30B9\u3068\u306E\u3084\u308A\u53D6\u308A\u3092\u3059\u308B\u3068\u304D\
  \u306B\u7279\u306B\u6709\u7528\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u30C7\u30FC\u30BF\u53CE\u96C6\u3092\u81EA\u52D5\u5316\u3057\u305F\u308A\
  \u3001Web\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u64CD\u4F5C\u3057\u305F\u308A\u3001\
  Google\u2026"
lastmod: 2024-02-19 22:05:00.711767
model: gpt-4-0125-preview
summary: "Google Apps Script\u3067HTML\u3092\u89E3\u6790\u3059\u308B\u3068\u306F\u3001\
  HTML\u30B3\u30F3\u30C6\u30F3\u30C4\u304B\u3089\u30C7\u30FC\u30BF\u3092\u62BD\u51FA\
  \u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u308C\u306F\
  \u3001Web\u30DA\u30FC\u30B8\u3084Web\u30D9\u30FC\u30B9\u306E\u30C7\u30FC\u30BF\u30BD\
  \u30FC\u30B9\u3068\u306E\u3084\u308A\u53D6\u308A\u3092\u3059\u308B\u3068\u304D\u306B\
  \u7279\u306B\u6709\u7528\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u30C7\u30FC\u30BF\u53CE\u96C6\u3092\u81EA\u52D5\u5316\u3057\u305F\u308A\u3001\
  Web\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u64CD\u4F5C\u3057\u305F\u308A\u3001Google\u2026"
title: "HTML\u306E\u89E3\u6790"
---

{{< edit_this_page >}}

## 何となぜ？
Google Apps ScriptでHTMLを解析するとは、HTMLコンテンツからデータを抽出することを意味します。これは、WebページやWebベースのデータソースとのやり取りをするときに特に有用です。プログラマーは、データ収集を自動化したり、Webコンテンツを操作したり、Google Apps（SheetsやDocsなど）とWeb機能を統合したりするためにこれを行います。

## 方法：
Google Apps Scriptには、HTMLを解析するための内蔵メソッドはありません。ただし、HTMLコンテンツを取得するために`UrlFetchApp`サービスを利用し、その後にJavaScriptのメソッドや正規表現（regex）を使用して解析することができます。以下は、Webページからtitleタグをフェッチして解析する基本的な例です。

```javascript
function parseHTMLTitle(url) {
  // WebページのHTMLコンテンツをフェッチする
  const response = UrlFetchApp.fetch(url);
  const htmlContent = response.getContentText();

  // 単純なregexを使用して<title>タグの内容を見つける
  const titleRegex = /<title>(.*?)<\/title>/;
  const match = htmlContent.match(titleRegex);

  // タイトルが見つかった場合は返す
  if (match && match.length > 1) {
    return match[1];
  }

  return 'タイトルが見つかりません';
}

// 使用例
const url = 'http://example.com';
const pageTitle = parseHTMLTitle(url);
Logger.log(pageTitle); // Webページのタイトルを出力
```

より洗練されたHTML解析には、HTMLをXMLとして解析するために`XmlService`を使用できます。ただし、これはHTMLが整形式のXMLである必要がありますが、常にそうであるわけではないことに注意してください：

```javascript
function parseHTMLUsingXmlService(htmlContent) {
  try {
    const document = XmlService.parse(htmlContent);
    const rootElement = document.getRootElement();
    // ここから、XmlServiceのメソッドを使用してXMLツリーをナビゲート
    // たとえば、特定の要素や属性を見つける例
  } catch(e) {
    Logger.log('HTML解析エラー: ' + e.toString());
  }
}
```

## 徹底解析：
歴史的に、Google Apps Scriptのような環境でのHTML解析は、Document Object Model（DOM）や他のプログラミングコンテキストで一般的な専用の解析ライブラリの不足のために困難でした。たとえば、ブラウザ内のJavaScriptにはDOMがすぐに使えるし、Node.jsの環境では`cheerio`や`jsdom`といったHTMLを解析するための豊富なNPMパッケージにアクセスできます。

Google Apps Scriptのアプローチは、`UrlFetchApp`を使用してWebリクエストを行い、その後、正規表現やXML解析メソッドを使って応答データを操作することに大きく依存しています。正規表現は単純な解析タスクには有用ですが、エラーのリスクとコードの脆弱性の可能性のため、複雑なHTMLに対しては一般的に推奨されません。`XmlService`によるXML解析はより構造化されたアプローチを提供しますが、整形式のHTML/XMLが必要であり、任意のWebページを扱う際に制限となることがあります。

複雑な解析ニーズがある場合や、不適切な形式のHTMLを扱う場合、Google Apps Scriptの外でWebサービスを使用するという代替戦略が考えられます。このサービスはHTMLコンテンツを処理し、より堅牢な解析技術やライブラリを使用して処理し、その後、Google Apps Scriptによって簡単に使用できる形式で処理されたデータを返すことができます。ただし、このアプローチはネットワーク遅延と追加のWebサービスの管理の複雑さを導入します。

これらの課題にもかかわらず、Google Apps Script内でのHTML解析は、他のGoogleサービスやAPIと組み合わせることで特に有力なツールとなり、生産性とデータ処理能力を大幅に向上させることができる多くの自動化可能性を提供します。
