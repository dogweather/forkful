---
date: 2024-01-26 03:40:35.561977-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\
  \u304F\u3068\u3044\u3046\u3053\u3068\u306F\u3001\u30C7\u30FC\u30BF\u306E\u89E3\u6790\
  \u3084JSON\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306E\u69CB\u7BC9\u6642\u306B\u30B3\
  \u30FC\u30C9\u3092\u53F0\u7121\u3057\u306B\u3059\u308B\u53EF\u80FD\u6027\u304C\u3042\
  \u308B\u7169\u308F\u3057\u3044\u5F15\u7528\u7B26\u3092\u9664\u53BB\u3059\u308B\u3053\
  \u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u5165\u529B\u3092\u6E05\u6F54\u306B\u4FDD\u3064\u3001\u69CB\u6587\u30A8\u30E9\
  \u30FC\u3092\u907F\u3051\u308B\u3001\u305D\u3057\u3066\u6587\u5B57\u5217\u3092\u4ED6\
  \u306E\u30B3\u30FC\u30C9\u90E8\u5206\u3068\u4E0A\u624B\u304F\u9023\u643A\u3055\u305B\
  \u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.659117-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\
  \u304F\u3068\u3044\u3046\u3053\u3068\u306F\u3001\u30C7\u30FC\u30BF\u306E\u89E3\u6790\
  \u3084JSON\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306E\u69CB\u7BC9\u6642\u306B\u30B3\
  \u30FC\u30C9\u3092\u53F0\u7121\u3057\u306B\u3059\u308B\u53EF\u80FD\u6027\u304C\u3042\
  \u308B\u7169\u308F\u3057\u3044\u5F15\u7528\u7B26\u3092\u9664\u53BB\u3059\u308B\u3053\
  \u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u5165\u529B\u3092\u6E05\u6F54\u306B\u4FDD\u3064\u3001\u69CB\u6587\u30A8\u30E9\
  \u30FC\u3092\u907F\u3051\u308B\u3001\u305D\u3057\u3066\u6587\u5B57\u5217\u3092\u4ED6\
  \u306E\u30B3\u30FC\u30C9\u90E8\u5206\u3068\u4E0A\u624B\u304F\u9023\u643A\u3055\u305B\
  \u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 方法：
例えば、`"\"Hello, World!\""`のように二重引用符で囲まれた文字列があって、引用符のない純粋なテキストを得たいとします。ここにその引用符の束縛から文字列を解放するJavaScriptの簡単なスニペットがあります：

```javascript
let quotedString = "\"Hello, World!\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // 出力: Hello, World!
```

そして、もしあなたがシングルクォートを扱っている場合は？正規表現を少し変えてみましょう：

```javascript
let singleQuotedString = "'Hello, World!'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // 出力: Hello, World!
```

また、あなたの文字列が両方の引用符を混ぜている場合はどうでしょう？問題ありません：

```javascript
let mixedQuotedString = "\"'Hello, World!'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // 出力: 'Hello, World!'
```

## 深堀り
JSONが主流になる前、エスケープ引用符はバックスラッシュや策略の荒野でした。初期のプログラミング言語は常に引用符と仲良くするわけではなく、多くの手作業による文字列操作を意味しました。今では、標準化されたデータ形式により、引用符を取り除くことは、JSONとして処理される前に入力をクリーンアップするか、またはフォーマットの衝突なしにテキストを保存することについてよくあります。

`.replace()`の代替方法？もちろんあります！引用符で文字列を分割して結合する、引用符の位置が確実に分かっている場合はsliceを使う、または必要なテキストを抽出するために正規表現マッチを使うなどができます。すべては文脈に依存します。

しかし、引用符内の引用符、エスケープされた引用符、そして国際文字など、エッジケースを忘れないでください。文字列を例外の潜在的な地雷原と考え、注意深く進んでください。現代のJavaScriptエンジンは正規表現操作を効率的に処理するよう最適化されているため、一般的にはこれが主流ですが、重いデータ処理タスクでのパフォーマンスは常にチェックする価値があります。

## 参照
文字列操作と正規表現に関するさらなる探求：

- Mozilla Developer NetworkのString.replace()について：https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101で正規表現パターンをテストする：https://regex101.com/
- 現代のWeb開発でなぜ多くの引用符を扱うかについて理解するためのJSON.org：http://json.org/
