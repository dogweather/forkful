---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:01.143972-07:00
description: "\u4F7F\u3044\u65B9: Bash\u3067\u306F\u3001`date`\u30B3\u30DE\u30F3\u30C9\
  \u304C\u73FE\u5728\u306E\u65E5\u4ED8\u3068\u6642\u523B\u3092\u53D6\u5F97\u3059\u308B\
  \u305F\u3081\u306E\u4E3B\u8981\u306A\u30C4\u30FC\u30EB\u3067\u3059\u3002\u3053\u308C\
  \u3092\u4F7F\u7528\u3059\u308B\u65B9\u6CD5\u306E\u3044\u304F\u3064\u304B\u306E\u4F8B\
  \u3092\u4EE5\u4E0B\u306B\u793A\u3057\u307E\u3059: 1. **\u30C7\u30D5\u30A9\u30EB\u30C8\
  \u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3068\u6642\
  \u523B\u3092\u53D6\u5F97\u3059\u308B:**."
lastmod: '2024-03-13T22:44:42.389125-06:00'
model: gpt-4-0125-preview
summary: "Bash\u3067\u306F\u3001`date`\u30B3\u30DE\u30F3\u30C9\u304C\u73FE\u5728\u306E\
  \u65E5\u4ED8\u3068\u6642\u523B\u3092\u53D6\u5F97\u3059\u308B\u305F\u3081\u306E\u4E3B\
  \u8981\u306A\u30C4\u30FC\u30EB\u3067\u3059\u3002\u3053\u308C\u3092\u4F7F\u7528\u3059\
  \u308B\u65B9\u6CD5\u306E\u3044\u304F\u3064\u304B\u306E\u4F8B\u3092\u4EE5\u4E0B\u306B\
  \u793A\u3057\u307E\u3059."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 使い方:
Bashでは、`date`コマンドが現在の日付と時刻を取得するための主要なツールです。これを使用する方法のいくつかの例を以下に示します:

1. **デフォルトフォーマットで現在の日付と時刻を取得する:**

```bash
date
```

*サンプル出力:*
```
Wed Apr 5 14:22:04 PDT 2023
```

2. **出力フォーマットをカスタマイズする:** 出力フォーマットは、`+%`形式指定子を使用して指定できます。たとえば、日付をYYYY-MM-DD形式で表示するには:

```bash
date "+%Y-%m-%d"
```

*サンプル出力:*
```
2023-04-05
```

3. **現在のUNIXタイムスタンプを取得する:** UNIXタイムスタンプは、Unix Epoch（1970年1月1日）以来の秒数です。これは、時間差に基づいて計算を行うスクリプトにとって便利です。

```bash
date "+%s"
```

*サンプル出力:*
```
1672877344
```

この基本操作には、通常、サードパーティのライブラリが使用されません。というのも、組み込みの`date`コマンドが包括的な機能を提供しているからです。しかし、より高度な日付と時刻の操作については、日付の算術や解析のためのライブラリを提供する他のプログラミング言語やツール、例えばPythonの`datetime`モジュールを使用する場合があります。
