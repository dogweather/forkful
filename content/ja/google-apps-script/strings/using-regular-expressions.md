---
title:                "正規表現の使用"
aliases:
- ja/google-apps-script/using-regular-expressions.md
date:                  2024-02-01T22:04:58.045725-07:00
model:                 gpt-4-0125-preview
simple_title:         "正規表現の使用"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/using-regular-expressions.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## はじめに: なぜ正規表現を使うのか？

正規表現（regex）は、文字列内の文字の組み合わせを検索するために使用されるパターンです。プログラマーは、テキストやデータの検索、編集、または操作にこれを利用し、パターンマッチングやデータ解析のタスクにとって不可欠です。

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
