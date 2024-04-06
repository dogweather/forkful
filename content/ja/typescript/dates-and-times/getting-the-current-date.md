---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:11.216375-07:00
description: "\u65B9\u6CD5\uFF1A TypeScript\u3067\u306F\u3001`Date`\u30AA\u30D6\u30B8\
  \u30A7\u30AF\u30C8\u3092\u4F7F\u7528\u3057\u3066\u73FE\u5728\u306E\u65E5\u4ED8\u3068\
  \u6642\u523B\u3092\u53D6\u5F97\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306E\u65B9\
  \u6CD5\u3067\u5B9F\u884C\u3067\u304D\u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:37:50.068091-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A TypeScript\u3067\u306F\u3001`Date`\u30AA\u30D6\u30B8\u30A7\
  \u30AF\u30C8\u3092\u4F7F\u7528\u3057\u3066\u73FE\u5728\u306E\u65E5\u4ED8\u3068\u6642\
  \u523B\u3092\u53D6\u5F97\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306E\u65B9\u6CD5\
  \u3067\u5B9F\u884C\u3067\u304D\u307E\u3059\uFF1A."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

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
