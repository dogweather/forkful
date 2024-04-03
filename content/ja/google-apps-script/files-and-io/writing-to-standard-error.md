---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:24.493539-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u306B\u304A\u3051\
  \u308B\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\u306E\u66F8\u304D\u8FBC\
  \u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\u8A3A\u65AD\
  \u3092\u901A\u5E38\u306E\u51FA\u529B\uFF08stdout\uFF09\u3068\u306F\u5225\u306E\u30B9\
  \u30C8\u30EA\u30FC\u30E0\u3078\u5C0E\u304F\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u6B63\u898F\u306E\u30D7\u30ED\u30B0\u30E9\u30E0\u51FA\u529B\u3068\u30A8\u30E9\
  \u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u533A\u5225\u3057\u3001\u30C7\u30D0\u30C3\
  \u30B0\u3084\u30ED\u30B0\u5206\u6790\u3092\u3088\u308A\u7C21\u5358\u306B\u884C\u3046\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.467094-06:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u306B\u304A\u3051\
  \u308B\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\u306E\u66F8\u304D\u8FBC\
  \u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\u8A3A\u65AD\
  \u3092\u901A\u5E38\u306E\u51FA\u529B\uFF08stdout\uFF09\u3068\u306F\u5225\u306E\u30B9\
  \u30C8\u30EA\u30FC\u30E0\u3078\u5C0E\u304F\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u6B63\u898F\u306E\u30D7\u30ED\u30B0\u30E9\u30E0\u51FA\u529B\u3068\u30A8\u30E9\
  \u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u533A\u5225\u3057\u3001\u30C7\u30D0\u30C3\
  \u30B0\u3084\u30ED\u30B0\u5206\u6790\u3092\u3088\u308A\u7C21\u5358\u306B\u884C\u3046\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002."
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
