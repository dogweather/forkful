---
date: 2024-01-26 03:40:35.561977-07:00
description: "\u65B9\u6CD5\uFF1A \u4F8B\u3048\u3070\u3001`\"\\\"Hello, World!\\\"\"\
  `\u306E\u3088\u3046\u306B\u4E8C\u91CD\u5F15\u7528\u7B26\u3067\u56F2\u307E\u308C\u305F\
  \u6587\u5B57\u5217\u304C\u3042\u3063\u3066\u3001\u5F15\u7528\u7B26\u306E\u306A\u3044\
  \u7D14\u7C8B\u306A\u30C6\u30AD\u30B9\u30C8\u3092\u5F97\u305F\u3044\u3068\u3057\u307E\
  \u3059\u3002\u3053\u3053\u306B\u305D\u306E\u5F15\u7528\u7B26\u306E\u675F\u7E1B\u304B\
  \u3089\u6587\u5B57\u5217\u3092\u89E3\u653E\u3059\u308BJavaScript\u306E\u7C21\u5358\
  \u306A\u30B9\u30CB\u30DA\u30C3\u30C8\u304C\u3042\u308A\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.450843-06:00'
model: gpt-4-0125-preview
summary: ''
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
