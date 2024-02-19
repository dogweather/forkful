---
aliases:
- /ja/typescript/getting-the-current-date/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:11.216375-07:00
description: "TypeScript\u306FJavaScript\u306B\u57FA\u3065\u3044\u305F\u8A00\u8A9E\
  \u3067\u3042\u308A\u3001\u73FE\u5728\u306E\u65E5\u4ED8\u3068\u6642\u523B\u60C5\u5831\
  \u306B\u30A2\u30AF\u30BB\u30B9\u3057\u3066\u64CD\u4F5C\u3059\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\u30BF\u30A4\u30E0\u30B9\u30BF\
  \u30F3\u30D7\u306E\u4F5C\u6210\u3001\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\
  \u3001\u305D\u306E\u4ED6\u306E\u6642\u9593\u306B\u654F\u611F\u306A\u6A5F\u80FD\u3092\
  \u5B9F\u88C5\u3059\u308B\u305F\u3081\u306B\u3001\u3057\u3070\u3057\u3070\u3053\u306E\
  \u6A5F\u80FD\u3092\u5FC5\u8981\u3068\u3057\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.691936
model: gpt-4-0125-preview
summary: "TypeScript\u306FJavaScript\u306B\u57FA\u3065\u3044\u305F\u8A00\u8A9E\u3067\
  \u3042\u308A\u3001\u73FE\u5728\u306E\u65E5\u4ED8\u3068\u6642\u523B\u60C5\u5831\u306B\
  \u30A2\u30AF\u30BB\u30B9\u3057\u3066\u64CD\u4F5C\u3059\u308B\u3053\u3068\u304C\u3067\
  \u304D\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A2\u30D7\
  \u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\
  \u30D7\u306E\u4F5C\u6210\u3001\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\
  \u305D\u306E\u4ED6\u306E\u6642\u9593\u306B\u654F\u611F\u306A\u6A5F\u80FD\u3092\u5B9F\
  \u88C5\u3059\u308B\u305F\u3081\u306B\u3001\u3057\u3070\u3057\u3070\u3053\u306E\u6A5F\
  \u80FD\u3092\u5FC5\u8981\u3068\u3057\u307E\u3059\u3002"
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
---

{{< edit_this_page >}}

## 何となぜ？
TypeScriptはJavaScriptに基づいた言語であり、現在の日付と時刻情報にアクセスして操作することができます。プログラマーは、アプリケーション内でタイムスタンプの作成、スケジューリング、その他の時間に敏感な機能を実装するために、しばしばこの機能を必要とします。

## 方法：
TypeScriptでは、`Date`オブジェクトを使用して現在の日付と時刻を取得できます。以下の方法で実行できます：

```typescript
const currentDate = new Date();
console.log(currentDate);
```

サンプル出力：
```
2023-04-12T07:20:50.52Z
```

このコードスニペットは、現在の日付と時刻を含む新しい`Date`オブジェクトを作成し、これをコンソールに出力します。また、toLocaleDateString()を使用して日付をより読みやすい形式でフォーマットすることもできます：

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

サンプル出力：
```
4/12/2023
```

### date-fnsの使用
より広範な日付の操作とフォーマットには、`date-fns`ライブラリが人気の選択です。まず、npm経由でインストールします：

```bash
npm install date-fns
```

その後、現在の日付をフォーマットするためにそれを使用できます：

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

サンプル出力：
```
2023-04-12
```

この`date-fns`の例では、現在の日付を"YYYY-MM-DD"形式の文字列としてフォーマットします。このライブラリは日付の操作に関する豊富な関数を提供しており、日付を扱う任意のTypeScriptプログラマにとって多様なツールを提供します。
