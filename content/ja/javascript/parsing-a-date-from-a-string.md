---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となく？（What & Why?）

日付解析とは、文字列から日付を取りだすプロセスのことを指します。プログラマがこれを行う主な理由は、データを活用しやすい形に変換し、日付としても扱えるようになるからです。

## どうやって？（How to:）

```Javascript
let dateStr = "2021-10-13";
let date = new Date(dateStr);

console.log(date);
```

出力:

```Javascript
2021-10-13T00:00:00.000Z
```

## 詳細な情報（Deep Dive）

1. 歴史的背景：初期のJavaScriptエンジンは特定の日付文字列形式への対応が不十分で、日付の解析は開発者を悩ませる問題でした。しかし、現代のJavaScriptエンジンはISO 8601日付文字列であれば簡単に解析できます。

2. 代替方法：一部のライブラリ（如、moment.jsやdate-fns等）では、独自の日付解析機能を提供しています。これらを使用すると、多くの形式の日付文字列を解析できます。

3. 実装詳細：`new Date(string)`は、与えられた文字列をECMA-262仕様に従って日付オブジェクトに解析します。この内部の動作はブラウザのJavaScriptエンジンに依存します。

## 参照元（See Also）

- [MDN Web Docs: Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js公式ドキュメンテーション](https://momentjs.com/)
- [date-fns公式ドキュメンテーション](https://date-fns.org/)