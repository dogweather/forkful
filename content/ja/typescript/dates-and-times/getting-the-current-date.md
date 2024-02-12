---
title:                "現在の日付の取得"
aliases:
- /ja/typescript/getting-the-current-date.md
date:                  2024-02-03T19:11:11.216375-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
