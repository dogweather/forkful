---
date: 2024-01-20 17:43:09.941462-07:00
description: "How to: (\u65B9\u6CD5) \u5B9F\u884C\u7D50\u679C\u306F\u6570\u5B57\u3092\
  \u53D6\u308A\u9664\u3044\u305F `Hello, World! This is a test. End.` \u3067\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:37:50.037057-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u5B9F\u884C\u7D50\u679C\u306F\u6570\u5B57\u3092\u53D6\u308A\
  \u9664\u3044\u305F `Hello, World!."
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

## How to: (方法)
```TypeScript
function deleteMatchingPattern(text: string, regex: RegExp): string {
    return text.replace(regex, '');
}

// 使用例
const originalText = "Hello, 123 World! This is a test. 456 End.";
const cleanedText = deleteMatchingPattern(originalText, /\d+/g);
console.log(cleanedText);  // "Hello,  World! This is a test.  End."
```

実行結果は数字を取り除いた `Hello,  World! This is a test.  End.` です。

## Deep Dive (深堀り)
文字パターン削除はソフトウェア開発の初期から使われています。最も代表的なツールは正規表現(RegExp)です。正規表現は検索と置換のための強力なパターンマッチング機能を提供します。`String.prototype.replace()` メソッドは、テキスト内のパターンにマッチする部分を別の文字列で置き換えるという場合によく使われます。削除は何も置き換えない（''）ことで実現されます。

別の方法としては、ループや条件分岐を使って文字列をスキャンし、指定のパターンに合致しない文字だけで新しい文字列を組み立てることもできます。しかし、このアプローチは正規表現を使う方法に比べてコードが長く、複雑になりがちです。

正規表現のパフォーマンスは実装や使用するパターンによって異なります。複雑なパターンや大量のテキストではパフォーマンスに影響を与える可能性があります。TypeScript を含む多くの現代のプログラミング言語は内部の正規表現エンジンを最適化することでこの問題に対処しています。

## See Also (関連情報)
- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Handbook - Everyday Types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html)
- [RegExp.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
