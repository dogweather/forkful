---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:58.045725-07:00
description: "\u4F7F\u3044\u65B9: Google Apps Script\u3067\u306E\u6B63\u898F\u8868\
  \u73FE\u306E\u4F7F\u7528\u306F\u3001JavaScript\u30D9\u30FC\u30B9\u306E\u69CB\u6587\
  \u306E\u304A\u304B\u3052\u3067\u76F4\u611F\u7684\u3067\u3059\u3002\u6B21\u306E\u3088\
  \u3046\u306B\u3057\u3066\u3001\u691C\u7D22\u3084\u30C7\u30FC\u30BF\u691C\u8A3C\u306A\
  \u3069\u306E\u4E00\u822C\u7684\u306A\u30BF\u30B9\u30AF\u306B\u30B9\u30AF\u30EA\u30D7\
  \u30C8\u3067regex\u3092\u7D44\u307F\u8FBC\u3080\u3053\u3068\u304C\u3067\u304D\u307E\
  \u3059\u3002 #."
lastmod: '2024-03-13T22:44:41.430476-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u3067\u306E\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528\
  \u306F\u3001JavaScript\u30D9\u30FC\u30B9\u306E\u69CB\u6587\u306E\u304A\u304B\u3052\
  \u3067\u76F4\u611F\u7684\u3067\u3059\u3002\u6B21\u306E\u3088\u3046\u306B\u3057\u3066\
  \u3001\u691C\u7D22\u3084\u30C7\u30FC\u30BF\u691C\u8A3C\u306A\u3069\u306E\u4E00\u822C\
  \u7684\u306A\u30BF\u30B9\u30AF\u306B\u30B9\u30AF\u30EA\u30D7\u30C8\u3067regex\u3092\
  \u7D44\u307F\u8FBC\u3080\u3053\u3068\u304C\u3067\u304D\u307E\u3059."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 使い方:
Google Apps Scriptでの正規表現の使用は、JavaScriptベースの構文のおかげで直感的です。次のようにして、検索やデータ検証などの一般的なタスクにスクリプトでregexを組み込むことができます。

### 文字列の検索
特定のパターン、例えばメールアドレスが文字列に含まれているかどうかを見つけたいとします。こちらが簡単な例です:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("見つかった: " + found[0]);
  } else {
    Logger.log("メールが見つかりませんでした。");
  }
}

// 使用例
findEmailInText("お問い合わせは info@example.com まで。");
```

### データ検証
正規表現はデータ検証で輝きます。以下は、入力文字列がシンプルなパスワードポリシー（少なくとも1つの大文字、1つの小文字、および最低8文字）に従っているかどうかをチェックする関数です。

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// 出力例
Logger.log(validatePassword("Str0ngPass")); // 出力: true
Logger.log(validatePassword("weak"));       // 出力: false
```

## 深掘り
Google Apps Scriptの正規表現はJavaScriptから継承され、1997年6月にECMAScript言語仕様として初めて標準化されました。強力ながら、複雑なパターンマッチングタスクに使用された場合や過度に使用されると、コードが混乱し、保守が困難になることがあります。これらは、他の解析方法を通じてより効率的に解決できるかもしれません。

例えば、HTMLやXMLの解析にregexを使用することはできますが、これらのドキュメントの入れ子になった複雑な構造のため、一般的に推奨されません。代わりに、HTMLに対するDOMパーサーのように、そのような構造を解析するために特別に設計されたツールの方が信頼性が高く、読みやすいです。

さらに、Google Apps Scriptの開発者は、大規模なテキスト操作タスクで複雑なregexパターンを使用する場合のパフォーマンスの問題に注意するべきです。なぜなら、regex処理はCPUを大きく消費することがあるからです。そのような場合、タスクをよりシンプルなサブタスクに分割するか、組み込みの文字列操作関数を使用することが、パフォーマンスと保守性のバランスをよりよく提供するでしょう。
