---
title:                "標準エラーへの書き込み"
date:                  2024-02-01T22:09:24.493539-07:00
model:                 gpt-4-0125-preview
simple_title:         "標準エラーへの書き込み"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/writing-to-standard-error.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

プログラミング言語における標準エラー（stderr）への書き込みは、エラーメッセージや診断を通常の出力（stdout）とは別のストリームへ導くことを意味します。これにより、プログラマーは正規のプログラム出力とエラーメッセージを区別し、デバッグやログ分析をより簡単に行うことができます。

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
