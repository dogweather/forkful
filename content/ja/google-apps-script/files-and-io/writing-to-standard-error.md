---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:24.493539-07:00
description: "\u65B9\u6CD5\uFF1A Google Apps Script\u306F\u3001Google Apps\u30D7\u30E9\
  \u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u5185\u3067\u8EFD\u91CF\u30A2\u30D7\u30EA\u30B1\
  \u30FC\u30B7\u30E7\u30F3\u958B\u767A\u7528\u306E\u30B9\u30AF\u30EA\u30D7\u30C8\u8A00\
  \u8A9E\u3067\u3042\u308B\u305F\u3081\u3001Node.js\u3084Python\u3067\u898B\u3064\u304B\
  \u308B\u3088\u3046\u306A`console.error()`\u306E\u3088\u3046\u306A\u76F4\u63A5\u7684\
  \u306A\u7D44\u307F\u8FBC\u307F\u95A2\u6570\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\
  \u305B\u3093\u3002\u3057\u304B\u3057\u3001Google Apps\u2026"
lastmod: '2024-04-05T22:37:49.802579-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Google Apps Script\u306F\u3001Google Apps\u30D7\u30E9\
  \u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u5185\u3067\u8EFD\u91CF\u30A2\u30D7\u30EA\u30B1\
  \u30FC\u30B7\u30E7\u30F3\u958B\u767A\u7528\u306E\u30B9\u30AF\u30EA\u30D7\u30C8\u8A00\
  \u8A9E\u3067\u3042\u308B\u305F\u3081\u3001Node.js\u3084Python\u3067\u898B\u3064\u304B\
  \u308B\u3088\u3046\u306A`console.error()`\u306E\u3088\u3046\u306A\u76F4\u63A5\u7684\
  \u306A\u7D44\u307F\u8FBC\u307F\u95A2\u6570\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\
  \u305B\u3093\u3002\u3057\u304B\u3057\u3001Google Apps Script \u306E\u30ED\u30AE\u30F3\
  \u30B0\u30B5\u30FC\u30D3\u30B9\u3092\u4F7F\u7528\u3059\u308B\u304B\u3001\u30AB\u30B9\
  \u30BF\u30E0\u30A8\u30E9\u30FC\u51E6\u7406\u3092\u4F7F\u7528\u3057\u3066\u30A8\u30E9\
  \u30FC\u51FA\u529B\u3092\u7BA1\u7406\u30FB\u5206\u96E2\u3059\u308B\u3053\u3068\u3067\
  \u3001\u3053\u306E\u52D5\u4F5C\u3092\u6A21\u5023\u3067\u304D\u307E\u3059\u3002"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

## 方法：
Google Apps Scriptは、Google Appsプラットフォーム内で軽量アプリケーション開発用のスクリプト言語であるため、Node.jsやPythonで見つかるような`console.error()`のような直接的な組み込み関数を提供していません。しかし、Google Apps Script のロギングサービスを使用するか、カスタムエラー処理を使用してエラー出力を管理・分離することで、この動作を模倣できます。

### 例： `Logger`を使ったエラーメッセージの使用
```javascript
function logError() {
  try {
    // エラーを模擬
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("ゼロによる除算を試みた");
  } catch (e) {
    // ログにエラーメッセージを書き込む
    Logger.log('エラー: ' + e.message);
  }
}
```

`logError()`を実行すると、このエラーメッセージはGoogle Apps Scriptのログに書き込まれ、`表示 > ログ`で閲覧できます。これは正確にはstderrではありませんが、標準出力からエラーログを分離するという類似の目的を果たします。

### 高度な診断ログ
より高度なデバッグやエラーログのために、Stackdriver Logging（現在はGoogle CloudのOperations Suiteとして知られています）を使用できます。

```javascript
function advancedErrorLogging() {
  try {
    // 故意にエラーを引き起こす
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('遭遇したエラー: ', e.toString());
  }
}
```

これにより、エラーメッセージはStackdriver Loggingへ送られ、エラーレベルのログとして管理されます。Stackdriver/Google CloudのOperations Suiteの統合は、`Logger`と比較して、より細かく、検索可能なログソリューションを提供することに注意してください。

## 深く掘り下げる
Google Apps Scriptに専用の`stderr`ストリームがないことは、クラウドベースのスクリプト言語としてのその性質と起源を反映しています。従来のコンソールや端末ベースの出力（stdoutやstderrのような）は、あまり関連がありません。歴史的に、Google Apps Scriptは、Google Appsの機能をシンプルなスクリプトで強化するために設計されており、より複雑なプログラミング環境で利用可能な包括的な機能よりも使いやすさに重点を置いています。

それにもかかわらず、Google Apps Scriptがより洗練されたアプリケーション開発に向けて進化するにつれて、開発者はエラー処理とログの取り扱いにおいて、利用可能なサービス（Loggerなど）を利用し、Google CloudのOperations Suiteとの統合を採用するなどの創造的なアプローチを採用しています。これらの方法は、直接的なstderr実装ではありませんが、クラウド中心の環境でのエラー管理と診断ログにおける強力な代替手段を提供します。

重要なことに、これらの方法はGoogle Apps Scriptのエコシステム内でその目的を果たしながらも、従来のプログラミング環境と比較した際のプラットフォームの限界を強調しています。詳細で階層的なエラー処理戦略が必要な開発者にとっては、外部のログサービスとの統合や、より従来のstderrとstdoutの取り扱いを提供するGoogle Cloud Functionsの採用が好ましい場合があります。
